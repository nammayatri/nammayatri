"""MMI / MapMyIndia mock."""

from status_store import extract_path_ids, deep_merge


def handle(handler, path, body):
    path_ids = extract_path_ids(path)
    override_status, extra = handler._get_override("mmi", *path_ids)
    if "token" in path:
        handler._json({"access_token": "mock-mmi-token", "expires_in": 86400})
        return
    base = {"results": [], "status": override_status or "ok"}
    if extra:
        base = deep_merge(base, extra)
    handler._json(base)
