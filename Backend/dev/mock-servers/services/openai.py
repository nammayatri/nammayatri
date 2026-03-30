"""LLM / Azure OpenAI mock."""

from status_store import extract_path_ids, deep_merge


def handle(handler, path, body):
    path_ids = extract_path_ids(path)
    override_status, extra = handler._get_override("openai", *path_ids)
    base = {
        "choices": [{"message": {"content": "Mock AI response"}, "finish_reason": "stop"}],
        "usage": {"prompt_tokens": 10, "completion_tokens": 20},
    }
    if extra:
        base = deep_merge(base, extra)
    handler._json(base)
