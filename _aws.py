import json
import logging
import os
from asyncio import sleep

from aiobotocore.session import get_session
from aiolimiter import AsyncLimiter
from botocore.config import Config
from botocore.exceptions import ClientError

AWS_ACCESS_KEY_ID = os.environ.get("AWS_ACCESS_KEY_ID")
AWS_SECRET_ACCESS_KEY = os.environ.get("AWS_SECRET_ACCESS_KEY")

CONFIG = Config(retries={"max_attempts": 1, "mode": "standard"})

SESSION = get_session()

################################################################################


def _claude_payload(model: str, system_message: str, prompt: str) -> dict:
    """Create a payload for the given Claude model, system_message, and prompt."""
    if model == "anthropic.claude-v2:1":
        return {
            "prompt": "\n\nHuman:"
            + system_message
            + "\n\n"
            + prompt
            + "\n\n"
            + "#" * 80
            + "\n\nAssistant:",
            "max_tokens_to_sample": 500,
        }
    else:
        return {
            "anthropic_version": "bedrock-2023-05-31",
            "max_tokens": 500,
            "messages": [
                {
                    "role": "user",
                    "content": [{"type": "text", "text": prompt}],
                },
            ],
            "system": system_message,
        }


def _parse_claude(model: str, response: dict) -> str:
    """Parse the response from the given Claude model."""
    if model == "anthropic.claude-v2:1":
        return response.get("completion", "").strip()
    else:
        contents = response.get("content", [])
        return " ".join(
            [
                content.get("text", "")
                for content in contents
                if content.get("type") == "text"
            ]
        ).strip()


async def _chat_claude(
    model: str,
    system_message: str,
    prompt: str,
    request_limiter: AsyncLimiter,
    token_limiter: AsyncLimiter,
    max_retries: int = 4,
) -> str | None:
    """Call the given Anthropic model with the given prompt and system_message."""
    retries = 0
    wait_time = 1
    # Create the payload
    payload = _claude_payload(model, system_message, prompt)
    n_tokens = (len(prompt) + len(system_message)) // 4
    async with SESSION.create_client(
        "bedrock-runtime",
        region_name="us-east-1",
        aws_secret_access_key=AWS_SECRET_ACCESS_KEY,
        aws_access_key_id=AWS_ACCESS_KEY_ID,
        config=CONFIG,
    ) as client:
        while retries < max_retries:
            try:
                await token_limiter.acquire(n_tokens)
                async with request_limiter:
                    # Pass payload as JSON bytes
                    raw_response = await client.invoke_model(
                        body=json.dumps(payload), modelId=model
                    )

                    # Read the response as a string
                    async with raw_response["body"] as stream:
                        str_response = await stream.read()

                    # Convert the response to a JSON object
                    response = json.loads(str_response)

                    return _parse_claude(model, response)

            except ClientError as e:
                logging.error("AWS error: %s", e)
                if retries >= max_retries:
                    raise e
                retries += 1
                await sleep(wait_time)
                wait_time *= 2

        return None


################################################################################


async def _chat_mistral(
    model: str,
    system_message: str,
    prompt: str,
    request_limiter: AsyncLimiter,
    token_limiter: AsyncLimiter,
    max_retries: int = 4,
) -> str | None:
    """Call the given Mistral model with the given prompt and system_message."""
    retries = 0
    wait_time = 1
    n_tokens = (len(prompt) + len(system_message)) // 4
    # Create the payload
    payload = {
        "prompt": system_message + "\n\n" + prompt + "\n\n" + "#" * 80,
        "max_tokens": 500,
    }
    async with SESSION.create_client(
        "bedrock-runtime",
        region_name="us-east-1",
        aws_secret_access_key=AWS_SECRET_ACCESS_KEY,
        aws_access_key_id=AWS_ACCESS_KEY_ID,
        config=CONFIG,
    ) as client:
        while retries < max_retries:
            try:
                await token_limiter.acquire(n_tokens)
                async with request_limiter:
                    # Pass payload as JSON bytes
                    raw_response = await client.invoke_model(
                        body=json.dumps(payload), modelId=model
                    )

                    # Read the response as a string
                    async with raw_response["body"] as stream:
                        str_response = await stream.read()

                    # Convert the response to a JSON object
                    response = json.loads(str_response)

                    # Get the output from the response
                    outputs = response.get("outputs", [])
                    return outputs[0].get("text", "").strip() if outputs else ""

            except ClientError as e:
                logging.error("AWS error: %s", e)
                if retries >= max_retries:
                    raise e
                retries += 1
                await sleep(wait_time)
                wait_time *= 2

        return None


################################################################################


def _llama_payload(model: str, system_message: str, prompt: str) -> dict:
    """Create a payload for the given LLaMa model, system_message, and prompt."""
    return {
        "prompt": (
            "<|begin_of_text|><|start_header_id|>system<|end_header_id|>\n\n"
            f"{system_message}"
            "<|eot_id|><start_header_id|>user<|end_header_id|>\n\n"
            f"{prompt}"
            "<|eot_id|><|start_header_id|>assistant<|end_header_id|>"
        ),
        "max_gen_len": 500,
    }


async def _chat_llama(
    model: str,
    system_message: str,
    prompt: str,
    request_limiter: AsyncLimiter,
    token_limiter: AsyncLimiter,
    max_retries: int = 4,
) -> str | None:
    """Call the given LLaMa model with the given prompt and system_message."""
    retries = 0
    wait_time = 1
    n_tokens = (len(prompt) + len(system_message)) // 4
    # Create the payload
    payload = _llama_payload(model, system_message, prompt)
    async with SESSION.create_client(
        "bedrock-runtime",
        region_name="us-west-2",
        aws_secret_access_key=AWS_SECRET_ACCESS_KEY,
        aws_access_key_id=AWS_ACCESS_KEY_ID,
        config=CONFIG,
    ) as client:
        while retries < max_retries:
            try:
                await token_limiter.acquire(n_tokens)
                async with request_limiter:
                    # Pass payload as JSON bytes
                    raw_response = await client.invoke_model(
                        body=json.dumps(payload), modelId=model
                    )

                    # Read the response as a string
                    async with raw_response["body"] as stream:
                        str_response = await stream.read()

                    # Convert the response to a JSON object
                    response = json.loads(str_response)

                    # Get the output from the response
                    return response.get("generation", "").strip()

            except ClientError as e:
                logging.error("AWS error: %s", e)
                if retries >= max_retries:
                    raise e
                retries += 1
                await sleep(wait_time)
                wait_time *= 2

        return None
