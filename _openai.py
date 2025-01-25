import logging
import os
from asyncio import sleep

import openai
import tiktoken
from aiolimiter import AsyncLimiter

CLIENT = openai.AsyncOpenAI(
    api_key=os.environ["OPENAI_API_KEY"],
    organization=os.environ.get("OPENAI_API_ORG"),
)

################################################################################


async def _chat_gpt(
    model: str,
    system_message: str,
    prompt: str,
    request_limiter: AsyncLimiter,
    token_limiter: AsyncLimiter,
    max_retries: int = 4,
) -> str | None:
    """Call the given OpenAI model with the given prompt and system_message."""
    retries = 0
    wait_time = 1
    encoding = tiktoken.encoding_for_model(model)
    n_tokens = len(encoding.encode(prompt)) + len(encoding.encode(system_message))
    while retries < max_retries:
        try:
            await token_limiter.acquire(n_tokens)
            async with request_limiter:
                response = await CLIENT.chat.completions.create(
                    model=model,
                    messages=[
                        {"role": "system", "content": system_message},
                        {"role": "user", "content": prompt},
                    ],
                    response_format={"type": "json_object"},
                )
            return response.choices[0].message.content
        except openai.OpenAIError as e:
            logging.error("OpenAI error: %s", e)
            if retries >= max_retries:
                raise e
            retries += 1
            await sleep(wait_time)
            wait_time *= 2

    return None
