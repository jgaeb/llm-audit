#!/usr/bin/env python
"""Generate responses to prompts from the database using the specified model."""
import argparse
import asyncio
import logging
import sqlite3
import sys

import aiofiles
import aiosqlite
from aiolimiter import AsyncLimiter
from tqdm.asyncio import tqdm

import _aws
import _openai
import _ratelimiters

MODELS = [
    {
        "short-name": "gpt-3.5",
        "full-name": "gpt-3.5-turbo-0125",
    },
    {
        "short-name": "gpt-4",
        "full-name": "gpt-4-0125-preview",
    },
    {
        "short-name": "gpt-4o-mini",
        "full-name": "gpt-4o-mini-2024-07-18",
    },
    {
        "short-name": "gpt-4o",
        "full-name": "gpt-4o-2024-05-13",
    },
    {
        "short-name": "mistral-7b",
        "full-name": "mistral.mistral-7b-instruct-v0:2",
    },
    {
        "short-name": "mixtral-8x7b",
        "full-name": "mistral.mixtral-8x7b-instruct-v0:1",
    },
    {
        "short-name": "claude-2",
        "full-name": "anthropic.claude-v2:1",
    },
    {
        "short-name": "claude-sonnet-3.5",
        "full-name": "anthropic.claude-3-5-sonnet-20240620-v1:0",
    },
    {
        "short-name": "claude-sonnet",
        "full-name": "anthropic.claude-3-sonnet-20240229-v1:0",
    },
    {
        "short-name": "claude-haiku",
        "full-name": "anthropic.claude-3-haiku-20240307-v1:0",
    },
    {
        "short-name": "claude-instant",
        "full-name": "anthropic.claude-instant-v1",
    },
    {
        "short-name": "llama3-8b",
        "full-name": "meta.llama3-1-8b-instruct-v1:0",
    },
    {
        "short-name": "llama3-70b",
        "full-name": "meta.llama3-1-70b-instruct-v1:0",
    },
]


################################################################################


async def chat(
    chat_fn: callable,
    prompt_id: int,
    model: str,
    system_message: str,
    prompt: str,
    request_limiter: AsyncLimiter,
    token_limiter: AsyncLimiter,
    connection_limiter: AsyncLimiter,
    max_retries: int = 4,
) -> str | None:
    try:
        async with connection_limiter:
            raw_response = await chat_fn(
                model=model,
                system_message=system_message,
                prompt=prompt,
                request_limiter=request_limiter,
                token_limiter=token_limiter,
                max_retries=max_retries,
            )
            error = False
            error_message = None
    except Exception as e:
        logging.error("Unsolvable error: %s", e)
        raw_response = None
        error = True
        error_message = str(e)

    async with aiosqlite.connect("data.db") as db:
        await db.execute(
            """
            INSERT INTO requests (
                prompt_id, model, raw_response, error, error_message
            ) VALUES (
                :prompt_id, :model, :raw_response, :error, :error_message)
            """,
            {
                "prompt_id": prompt_id,
                "model": model,
                "raw_response": raw_response,
                "error": error,
                "error_message": error_message,
            },
        )
        await db.commit()


async def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--log-level", type=str, default="INFO")
    parser.add_argument("--log-file", type=str, default="chat.log")
    parser.add_argument("--n_max", type=int, default=100)
    parser.add_argument("model", type=str)
    parser.add_argument("experiment", type=str)
    args = parser.parse_args()

    # If the model doesn't exact match one of the models, print the model names
    # to stderr and exit with status 1
    if args.model not in [model["short-name"] for model in MODELS]:
        print("Available models:")
        for model in MODELS:
            print(model["short-name"], file=sys.stderr)
        sys.exit(1)

    # Get the full name of the model
    model = next(model for model in MODELS if model["short-name"] == args.model)[
        "full-name"
    ]

    # Determine if the model is openai, anthropic (claude), or mistral
    if "gpt" in model:
        chat_fn = _openai._chat_gpt
    elif "claude" in model:
        chat_fn = _aws._chat_claude
    elif "llama" in model:
        chat_fn = _aws._chat_llama
    else:
        chat_fn = _aws._chat_mistral

    # Get the limiters for the model
    request_limiter = _ratelimiters.REQUEST_LIMITER[model]
    token_limiter = _ratelimiters.TOKEN_LIMITER[model]
    connection_limiter = _ratelimiters.CONNECTION_LIMITER

    # Set up logging
    logging.basicConfig(
        level=args.log_level,
        filename=args.log_file,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    )

    # Pull all of the prompts for the experiment from the database
    with sqlite3.connect("data.db") as conn:
        conn.row_factory = sqlite3.Row
        prompts = conn.execute(
            """
            SELECT prompts.prompt_id, prompts.system_message, prompts.prompt
            FROM prompts
            LEFT JOIN (
                SELECT prompt_id
                FROM requests
                WHERE model = :model
            ) AS model_requests ON prompts.prompt_id = model_requests.prompt_id
            WHERE model_requests.prompt_id IS NULL
            AND prompts.experiment_type = :experiment
            LIMIT :n_max
            """,
            {"experiment": args.experiment, "model": model, "n_max": args.n_max},
        ).fetchall()

    # Create a list of chat coroutines
    tasks = [
        chat(
            chat_fn=chat_fn,
            prompt_id=prompt["prompt_id"],
            model=model,
            system_message=prompt["system_message"],
            prompt=prompt["prompt"],
            request_limiter=request_limiter,
            token_limiter=token_limiter,
            connection_limiter=connection_limiter,
        )
        for prompt in prompts
    ]

    # Run the chat coroutines
    await tqdm.gather(*tasks)


if __name__ == "__main__":
    asyncio.run(main())
