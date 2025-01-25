#!/usr/bin/env python3
import argparse
import asyncio
import os

import aiofiles
import aiosqlite
import openai
import tiktoken
from aiolimiter import AsyncLimiter
from tqdm.asyncio import tqdm_asyncio as tqdm

client = openai.AsyncOpenAI(
    api_key=os.environ["OPENAI_API_KEY"],
    organization=os.environ.get("OPENAI_API_ORG"),
)
encoding = tiktoken.get_encoding("cl100k_base")
semaphore = asyncio.Semaphore(100)
rate_limiter = AsyncLimiter(4000)


async def get_ids(db):
    async with db.execute(
        "SELECT interview_id FROM interviews WHERE in_study = 1"
    ) as cursor:
        return [str(row[0]) for row in await cursor.fetchall()]


async def get_question_ids(interview_id: str, redacted: bool):
    folder = "redacted" if redacted else "unredacted"
    ids = []
    for filename in os.listdir(f"questions/{folder}/"):
        if filename.startswith(f"{interview_id}_"):
            ids.append(filename[:-4].split("_")[1])
    return ids


async def embed(
    interview_id: str, redacted: bool, use_resume: bool, db: aiosqlite.Connection
) -> None:
    async with semaphore:
        text = ""
        if use_resume:
            folder = "redacted" if redacted else "unredacted"
            async with aiofiles.open(
                f"resumes/{folder}/{interview_id}.txt", "r"
            ) as file:
                text = await file.read()
        else:
            folder = "redacted" if redacted else "unredacted"
            question_ids = await get_question_ids(interview_id, redacted)
            for question_id in question_ids:
                async with aiofiles.open(
                    f"questions/{folder}/{interview_id}_{question_id}.txt", "r"
                ) as file:
                    text += "\n\n" + await file.read()

        tokens = encoding.encode(text)[:8178]
        text = encoding.decode(tokens)

        async with rate_limiter:
            response = await client.embeddings.create(
                input=[
                    text
                    + "\n\nWhat race and gender is the person who wrote or said this?"
                ],
                model="text-embedding-3-large",
                dimensions=256,
            )

        values = [interview_id, int(redacted), int(use_resume)] + response.data[
            0
        ].embedding
        placeholders = ",".join(["?"] * (3 + 256))
        await db.execute(
            f"""
            INSERT INTO embeddings
            (
                interview_id,
                redacted,
                resume,
                {','.join(f'X_{i}' for i in range(256))}
            )
            VALUES ({placeholders})
            """,
            values,
        )
        await db.commit()


async def main():
    async with aiosqlite.connect("data.db") as db:
        interview_ids = await get_ids(db)
        tasks = []
        for redacted in (False, True):
            for resume in (False, True):
                for id in interview_ids:
                    # Check if this combination already exists
                    async with db.execute(
                        """SELECT 1 FROM embeddings 
                        WHERE interview_id = ? AND redacted = ? AND resume = ?""",
                        (id, int(redacted), int(resume)),
                    ) as cursor:
                        if not await cursor.fetchone():
                            tasks.append(embed(id, redacted, resume, db))

        if not tasks:
            return

        results = await tqdm.gather(*tasks)
        for interview_id, embedding, redacted, is_resume in results:
            values = [interview_id, int(redacted), int(is_resume)] + embedding
            placeholders = ",".join(["?"] * (3 + 256))
            await db.execute(
                f"INSERT INTO embeddings (interview_id, redacted, resume, {','.join(f'X_{i}' for i in range(256))}) "
                f"VALUES ({placeholders})",
                values,
            )
            await db.commit()


if __name__ == "__main__":
    asyncio.run(main())
