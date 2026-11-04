"""Exotel call mock."""

from status_store import extract_path_ids, deep_merge


def handle(handler, path, body):
    path_ids = extract_path_ids(path)
    override_status, extra = handler._get_override("exotel", *path_ids)
    base = {
        "Call": {
            "Sid": "mock-call-sid",
            "Status": override_status or "queued",
        }
    }
    if extra:
        base = deep_merge(base, extra)
    handler._json(base)
