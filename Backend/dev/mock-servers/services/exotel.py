"""Exotel call mock."""

import uuid

from status_store import extract_path_ids, deep_merge


def handle(handler, path, body):
    path_ids = extract_path_ids(path)
    override_status, extra = handler._get_override("exotel", *path_ids)
    sid = "mock-call-" + uuid.uuid4().hex[:12]
    # every field the backend's ExotelInitiateCallRespBody decodes without a default
    base = {
        "Call": {
            "Sid": sid,
            "DateCreated": "2026-01-01 00:00:00",
            "DateUpdated": "2026-01-01 00:00:00",
            "AccountSid": "juspay2",
            "To": "",
            "From": "",
            "PhoneNumberSid": "mock",
            "Status": override_status or "queued",
            "StartTime": "2026-01-01 00:00:00",
            "Direction": "outbound-api",
            "Uri": "/v1/Accounts/juspay2/Calls/" + sid + ".json",
        }
    }
    if extra:
        base = deep_merge(base, extra)
    handler._json(base)
