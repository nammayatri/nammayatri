"""Mock SMS (MyValueFirst-compatible) — replaces the Haskell mock-sms-exe (port 4343).

Routes (mounted under any path containing "/sms"):
  GET   /sms/                          — health, returns "MockSms is UP"-equivalent JSON.
  POST  /sms/send                      — accept an SMS, store body keyed by recipient number.
  POST  /sms/                          — same as /send (MyValueFirst posts to the root).
  GET   /sms/read/<mobile_number>      — return the list of bodies sent to <mobile_number>.
  POST  /sms/clear                     — clear the in-memory store (for tests).

Recipient is taken from JSON `to` / `mobile` / `phone` fields, falling back to "unknown".
"""

import collections
import json
import threading
from urllib.parse import parse_qs

_LOCK = threading.Lock()
_MESSAGES = collections.defaultdict(list)


def _extract_recipient(body_json, query_params):
    if isinstance(body_json, dict):
        for k in ("to", "mobile", "phone", "destination_address", "destinationAddress"):
            v = body_json.get(k)
            if v:
                return str(v)
    for k in ("to", "destination_address"):
        if k in query_params and query_params[k]:
            return query_params[k][0]
    return None


def handle(handler, path, body):
    if path.endswith("/sms") or path.endswith("/sms/") or path.endswith("/health"):
        if handler.command == "GET":
            return handler._json({"status": "UP", "service": "mock-sms"})

    if path.endswith("/clear") and handler.command == "POST":
        with _LOCK:
            _MESSAGES.clear()
        return handler._json({"cleared": True})

    if "/read/" in path and handler.command == "GET":
        number = path.split("/read/", 1)[1].strip("/")
        with _LOCK:
            items = list(_MESSAGES.get(number, []))
        return handler._json(items)

    text = body.decode("utf-8") if isinstance(body, bytes) and body else (body or "")
    body_json = None
    try:
        body_json = json.loads(text) if text else None
    except (json.JSONDecodeError, ValueError):
        body_json = None

    from urllib.parse import urlparse
    query = urlparse(handler.path).query
    qp = parse_qs(query) if query else {}

    recipient = _extract_recipient(body_json, qp) or "unknown"
    payload = body_json if body_json is not None else text
    with _LOCK:
        _MESSAGES[recipient].append(payload)

    return handler._json({
        "data": {"message": [{"id": f"mock-{recipient}-{len(_MESSAGES[recipient])}", "status": "SUCCESS"}]},
    })
