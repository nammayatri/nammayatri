"""OTP Transit / Nandi / GTFS mock."""

from status_store import extract_path_ids, deep_merge


def handle(handler, path, body):
    path_ids = extract_path_ids(path)
    override_status, extra = handler._get_override("transit", *path_ids)
    base = {"results": [], "status": override_status or "ok"}
    if extra:
        base = deep_merge(base, extra)
    handler._json(base)
