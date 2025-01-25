#!/usr/bin/env python
"""Extract ratings and demographics from imperfectly structured data."""
import argparse
import asyncio
import logging
import os
import sqlite3
from typing import Annotated, Optional

import aiosqlite
import openai
import tiktoken
from pydantic import BaseModel, Field, ValidationError, conint, field_validator
from tqdm.asyncio import tqdm

import _ratelimiters

client = openai.AsyncOpenAI(
    api_key=os.environ["OPENAI_API_KEY"],
    organization=os.environ.get("OPENAI_API_ORG"),
)
encoding = tiktoken.encoding_for_model("gpt-4o-mini")

semaphore = asyncio.Semaphore(100)

# True limits are 1e4 requests per minute and 2e6 tokens per minute
REQUEST_LIMITER = _ratelimiters.REQUEST_LIMITER["gpt-4o-mini-2024-07-18"]
TOKEN_LIMITER = _ratelimiters.TOKEN_LIMITER["gpt-4o-mini-2024-07-18"]
CONNECTION_LIMITER = _ratelimiters.CONNECTION_LIMITER

################################################################################

SYSTEM_RATINGS = """
You are a helpful JSON reformatting assistant. I give you potentially unstructed
data that contains the following fields rated from 1 to 5:
• Professionalism,
• Experience,
• Fit,
• Hire.
You extract the ratings and return them in the following JSON format:
```json
{
    "professionalism": int,
    "experience": int,
    "fit": int,
    "hire": int
}
```
If the same dimension is rated multiple times, you should return the largest
rating. If the rating is given as a decimal or fraction, you should round to the
nearest integer.

You may need to repair invalid JSON, fix mispelled field names, or extract the
ratings from a larger document. These are not errors. If you truly encounter an
error, please return an error message in the following JSON format:
```json
{
    "error": true,
    "error_message": str
}
Errors you may encounter include (but are not limited to):
• There is no text (write "Empty text" in the error_message),
• Some field is *actually* missing (write "Missing field" in the error_message).
```
""".strip()

RATINGS_N = len(encoding.encode(SYSTEM_RATINGS))

SYSTEM_CHECKS = """
You are a helpful JSON reformatting assistant. I give you potentially unstructed
data that contains the following fields:
• Race,
• Gender.
Race must be either "Asian", "Black", "Hispanic", or "White". Gender must be
either "male" or "female". You extract the demographics and return them in the
following JSON format:
```json
{
    "race": str,
    "gender": str
}
```
If the either field is repeated, you should return the last value. If the text
does not contain a valid value ("Asian", "Black", "Hispanic", "White"; "female",
"male"), you may return "NA" for that field.

You may need to repair invalid JSON, fix mispelled field names, or extract the
demographics from a larger document. These are not errors. If you truly
encounter an error, please return an error message in the following JSON format:
```json
{
    "error": true,
    "error_message": str
}
```
(Note that no text is not an error in this case: you should return NA for both
fields.)
"""

CHECKS_N = len(encoding.encode(SYSTEM_CHECKS))

################################################################################


class RatingResponse(BaseModel):
    """A rating response from the model."""

    summary: Optional[str] = None
    professionalism: conint(ge=1, le=5)
    experience: conint(ge=1, le=5)
    fit: conint(ge=1, le=5)
    hire: conint(ge=1, le=5)


class CheckResponse(BaseModel):
    """A demographic response from the model."""

    name: Optional[str] = None
    race: Annotated[str, Field(pattern=r"(?i)^(Asian|Black|Hispanic|White|NA)$")]
    gender: Annotated[str, Field(pattern=r"(?i)^(male|female|NA)$")]

    @field_validator("race")
    @classmethod
    def upper_case_race(cls, value):
        return value.capitalize()

    @field_validator("gender")
    @classmethod
    def lower_case_gender(cls, value):
        return value.lower()


class Error(BaseModel):
    """An error response from the model."""

    error: bool = True
    error_message: str


async def extract_rating(request_id: int, text: str, conn: aiosqlite.Connection):
    """Extract the ratings from the text."""
    # If there is no text, log an error, because there's a problem with the
    # request
    if not text:
        logging.error("Empty text for request_id: %s", request_id)
        async with conn.cursor() as cur:
            await cur.execute(
                """
                INSERT INTO ratings (request_id, error, error_message)
                VALUES (?, ?, ?);
                """,
                (request_id, True, "Empty text"),
            )
            await conn.commit()
        return

    # First, check if the text is already valid JSON
    try:
        response = RatingResponse.model_validate_json(text)
        logging.debug("Validated JSON: %s", text)
        async with CONNECTION_LIMITER:
            async with conn.cursor() as cur:
                await cur.execute(
                    """
                    INSERT INTO ratings (
                        request_id, professionalism, experience, fit, hire, parsed,
                        error
                    ) VALUES (?, ?, ?, ?, ?, ?, ?);
                    """,
                    (
                        request_id,
                        response.professionalism,
                        response.experience,
                        response.fit,
                        response.hire,
                        False,
                        False,
                    ),
                )
                await conn.commit()
                return

    # If the text is not valid JSON, we need to send it to the model
    except ValidationError:
        await TOKEN_LIMITER.acquire(
            len(encoding.encode(text, disallowed_special=())) + RATINGS_N
        )
        async with CONNECTION_LIMITER, REQUEST_LIMITER:
            try:
                raw_response = await client.chat.completions.create(
                    model="gpt-4o-mini-2024-07-18",
                    messages=[
                        {"role": "system", "content": SYSTEM_RATINGS},
                        {"role": "user", "content": text},
                    ],
                    response_format={"type": "json_object"},
                )
            except openai.BadRequestError as e:
                logging.error("Bad request with text: %s, error: %s", text, e)
                async with conn.cursor() as cur:
                    await cur.execute(
                        """
                        INSERT INTO ratings (request_id, error, error_message)
                        VALUES (?, ?, ?);
                        """,
                        (request_id, True, str(e)),
                    )
                    await conn.commit()
                    return
            str_response = raw_response.choices[0].message.content
            logging.debug("Model response: %s", str_response)

        try:
            response = RatingResponse.model_validate_json(str_response)
            async with conn.cursor() as cur:
                await cur.execute(
                    """
                    INSERT INTO ratings (
                        request_id, professionalism, experience, fit, hire, parsed,
                        error
                    ) VALUES (?, ?, ?, ?, ?, ?, ?);
                    """,
                    (
                        request_id,
                        response.professionalism,
                        response.experience,
                        response.fit,
                        response.hire,
                        True,
                        False,
                    ),
                )
                await conn.commit()
        except ValidationError:
            try:
                error = Error.model_validate_json(str_response)
                async with conn.cursor() as cur:
                    await cur.execute(
                        """
                        INSERT INTO ratings (request_id, error, error_message)
                        VALUES (?, ?, ?);
                        """,
                        (request_id, True, error.error_message),
                    )
                    await conn.commit()

            except ValidationError:
                error_message = f"Unknown error parsing model response: {str_response}"
                logging.error(error_message)
                async with conn.cursor() as cur:
                    await cur.execute(
                        """
                        INSERT INTO ratings (request_id, error, error_message)
                        VALUES (?, ?, ?);
                        """,
                        (request_id, True, error_message),
                    )
                    await conn.commit()


async def extract_checks(request_id: int, text: str, conn: aiosqlite.Connection):
    """Extract the manipulation check from the text."""
    # If there is no text, log an error, because there's a problem with the
    # request
    if not text:
        logging.error("Empty text for request_id: %s", request_id)
        async with conn.cursor() as cur:
            await cur.execute(
                """
                INSERT INTO ratings (request_id, error, error_message)
                VALUES (?, ?, ?);
                """,
                (request_id, True, "Empty text"),
            )
            await conn.commit()
        return

    # First, check if the text is already valid JSON
    try:
        response = CheckResponse.model_validate_json(text)
        logging.debug("Validated JSON: %s", text)
        async with CONNECTION_LIMITER:
            async with conn.cursor() as cur:
                await cur.execute(
                    """
                    INSERT INTO checks (request_id, race, gender, parsed, error)
                    VALUES (?, ?, ?, ?, ?);
                    """,
                    (request_id, response.race, response.gender, False, False),
                )
                await conn.commit()
                return

    # If the text is not valid JSON, we need to send it to the model
    except ValidationError:
        await TOKEN_LIMITER.acquire(
            len(encoding.encode(text, disallowed_special=())) + CHECKS_N
        )
        async with CONNECTION_LIMITER, REQUEST_LIMITER:
            try:
                raw_response = await client.chat.completions.create(
                    model="gpt-4o-mini-2024-07-18",
                    messages=[
                        {"role": "system", "content": SYSTEM_CHECKS},
                        {"role": "user", "content": text},
                    ],
                    response_format={"type": "json_object"},
                )
            except openai.BadRequestError as e:
                logging.error("Bad request with text: %s, error: %s", text, e)
                async with conn.cursor() as cur:
                    await cur.execute(
                        """
                        INSERT INTO checks (request_id, error, error_message)
                        VALUES (?, ?, ?);
                        """,
                        (request_id, True, str(e)),
                    )
                    await conn.commit()
                return
            str_response = raw_response.choices[0].message.content
            logging.debug("Model response: %s", str_response)

        try:
            response = CheckResponse.model_validate_json(str_response)
            async with conn.cursor() as cur:
                await cur.execute(
                    """
                    INSERT INTO checks (request_id, race, gender, parsed, error)
                    VALUES (?, ?, ?, ?, ?);
                    """,
                    (
                        request_id,
                        response.race,
                        response.gender,
                        True,
                        False,
                    ),
                )
                await conn.commit()
        except ValidationError:
            try:
                error = Error.model_validate_json(str_response)
                async with conn.cursor() as cur:
                    await cur.execute(
                        """
                        INSERT INTO checks (request_id, error, error_message)
                        VALUES (?, ?, ?);
                        """,
                        (request_id, True, error.error_message),
                    )
                    await conn.commit()

            except ValidationError:
                error_message = f"Unknown error parsing model response: {str_response}"
                logging.error(error_message)
                async with conn.cursor() as cur:
                    await cur.execute(
                        """
                        INSERT INTO checks (request_id, error, error_message)
                        VALUES (?, ?, ?);
                        """,
                        (request_id, True, error_message),
                    )
                    await conn.commit()


async def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--log-level", type=str, default="INFO")
    parser.add_argument("--log-file", type=str, default="extract.log")
    parser.add_argument("--n_max", type=int, default=100)
    parser.add_argument("kind", type=str, choices=["ratings", "checks"])
    args = parser.parse_args()

    # Set up logging
    logging.basicConfig(
        level=args.log_level,
        filename=args.log_file,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    )

    # Pull all the requests of the given kind from the database
    with sqlite3.connect("data.db") as conn:
        operator = "" if args.kind == "checks" else "NOT"
        conn.row_factory = sqlite3.Row
        requests = conn.execute(
            f"""
            SELECT requests.*
            FROM requests
            LEFT JOIN prompts
            ON requests.prompt_id = prompts.prompt_id
            LEFT JOIN {args.kind}
            ON requests.request_id = {args.kind}.request_id
            WHERE {operator} prompts.experiment_type = 'manipulation_check'
            AND {args.kind}.request_id IS NULL
            AND NOT requests.error
            LIMIT :n_max;
            """,
            {"n_max": args.n_max, "kind": args.kind},
        ).fetchall()

    # Create an async connection to the database
    conn = await aiosqlite.connect("data.db")

    # Create a list of extraction coroutines
    extract_ = extract_rating if args.kind == "ratings" else extract_checks
    tasks = [
        extract_(request["request_id"], request["raw_response"], conn)
        for request in requests
    ]

    # Run the extraction coroutines
    await tqdm.gather(*tasks)

    # Close the connection
    await conn.close()


if __name__ == "__main__":
    asyncio.run(main())
