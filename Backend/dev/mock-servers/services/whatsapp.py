"""WhatsApp providers mock (GupShup, Karix, Tata).

Response format matches OptApiResp:
  { "response": {"id": "...", "phone": "...", "details": "...", "status": "..."},
    "data": null }
"""

from status_store import extract_path_ids, deep_merge


def handle(handler, path, body):
    path_ids = extract_path_ids(path)
    override_status, extra = handler._get_override("whatsapp", *path_ids)
    base = {
        "response": {
            "id": "mock-wa-123",
            "phone": "",
            "details": "Submitted",
            "status": override_status or "success",
        },
        "data": None,
    }
    if extra:
        base = deep_merge(base, extra)
    handler._json(base)
