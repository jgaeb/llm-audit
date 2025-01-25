from asyncio import Semaphore

from aiolimiter import AsyncLimiter

CONNECTION_LIMITER = Semaphore(100)

REQUEST_LIMITER = {
    "gpt-3.5-turbo-0125": AsyncLimiter(500, 3),
    "gpt-4-0125-preview": AsyncLimiter(250, 3),
    "gpt-4o-mini-2024-07-18": AsyncLimiter(500, 3),
    "gpt-4o-2024-05-13": AsyncLimiter(250, 3),
    "anthropic.claude-v2:1": AsyncLimiter(5, 4),
    "anthropic.claude-3-5-sonnet-20240620-v1:0": AsyncLimiter(5, 6),
    "anthropic.claude-3-sonnet-20240229-v1:0": AsyncLimiter(5, 4),
    "anthropic.claude-3-haiku-20240307-v1:0": AsyncLimiter(15, 5),
    "anthropic.claude-instant-v1": AsyncLimiter(15, 5),
    "mistral.mistral-7b-instruct-v0:2": AsyncLimiter(30, 5),
    "mistral.mixtral-8x7b-instruct-v0:1": AsyncLimiter(15, 5),
    "meta.llama3-1-8b-instruct-v1:0": AsyncLimiter(20, 3),
    "meta.llama3-1-70b-instruct-v1:0": AsyncLimiter(10, 3),
}

TOKEN_LIMITER = {
    "gpt-3.5-turbo-0125": AsyncLimiter(500_000, 15),
    "gpt-4-0125-preview": AsyncLimiter(150_000, 15),
    "gpt-4o-mini-2024-07-18": AsyncLimiter(500_000, 15),
    "gpt-4o-2024-05-13": AsyncLimiter(500_000, 15),
    "anthropic.claude-v2:1": AsyncLimiter(12_500, 5),
    "anthropic.claude-3-5-sonnet-20240620-v1:0": AsyncLimiter(25_000, 5),
    "anthropic.claude-3-sonnet-20240229-v1:0": AsyncLimiter(12_500, 5),
    "anthropic.claude-3-haiku-20240307-v1:0": AsyncLimiter(18_750, 5),
    "anthropic.claude-instant-v1": AsyncLimiter(18_750, 5),
    "mistral.mistral-7b-instruct-v0:2": AsyncLimiter(18_750, 5),
    "mistral.mixtral-8x7b-instruct-v0:1": AsyncLimiter(18_750, 5),
    "meta.llama3-1-8b-instruct-v1:0": AsyncLimiter(12_000, 3),
    "meta.llama3-1-70b-instruct-v1:0": AsyncLimiter(12_000, 3),
}
