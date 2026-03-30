"""PayTM payment mock."""

from status_store import extract_path_ids, deep_merge


def handle(handler, path, body):
    path_ids = extract_path_ids(path)
    override_status, extra = handler._get_override("paytm", *path_ids)
    base = {
        "status": override_status or "SUCCESS",
        "resultInfo": {"resultCode": "0000"},
    }
    if extra:
        base = deep_merge(base, extra)
    handler._json(base)
