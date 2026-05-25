"""Mock FCM (Firebase Cloud Messaging) — replaces the Haskell mock-fcm-exe (port 4545).

Routes (mounted under any path containing "/fcm"):
  POST  /fcm/...                      — accept a notification, store it under the
                                        recipient token (from JSON body `to` or `message.token`).
                                        Returns the same envelope shape the FCM v1 API returns.
  GET   /fcm/read/<token>             — return the list of bodies sent to <token>, newest last.
  POST  /fcm/clear                    — clear the in-memory store (for tests).
"""

import collections
import json
import threading

_LOCK = threading.Lock()
_NOTIFICATIONS = collections.defaultdict(list)


def _extract_token(body_json):
    if not isinstance(body_json, dict):
        return None
    if "to" in body_json:
        return body_json["to"]
    msg = body_json.get("message") or {}
    if isinstance(msg, dict) and "token" in msg:
        return msg["token"]
    return None


def handle(handler, path, body):
    text = body.decode("utf-8") if isinstance(body, bytes) and body else (body or "")
    try:
        body_json = json.loads(text) if text else {}
    except (json.JSONDecodeError, ValueError):
        body_json = {}

    if path.endswith("/clear") and handler.command == "POST":
        with _LOCK:
            _NOTIFICATIONS.clear()
        return handler._json({"cleared": True})

    if "/read/" in path and handler.command == "GET":
        token = path.split("/read/", 1)[1].strip("/")
        with _LOCK:
            items = list(_NOTIFICATIONS.get(token, []))
        return handler._json(items)

    # Default: treat as a send. Echo a v1-shaped response.
    token = _extract_token(body_json) or "unknown"
    with _LOCK:
        _NOTIFICATIONS[token].append(body_json)
    return handler._json({"name": f"projects/mock/messages/{token}-{len(_NOTIFICATIONS[token])}"})
