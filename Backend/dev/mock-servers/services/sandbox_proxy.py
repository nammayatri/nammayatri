"""Sandbox passthrough proxy.

Forwards incoming requests for selected service prefixes (nandi, gtfs-inmemory)
to the real moving.tech sandbox upstream. The patch rules route these services
to the mock server (http://localhost:8080/<prefix>) so calls flow through the
mock hit-recorder, and this module relays them to the live sandbox.
"""

import urllib.error
import urllib.request
from urllib.parse import urlparse

UPSTREAM_BASE = "https://api.sandbox.moving.tech"

PREFIXES = ("/gtfs-inmemory", "/nandi")

_HOP_BY_HOP = {
    "host",
    "content-length",
    "connection",
    "keep-alive",
    "proxy-authenticate",
    "proxy-authorization",
    "te",
    "trailers",
    "transfer-encoding",
    "upgrade",
    "accept-encoding",
}


def _match_prefix(path):
    for prefix in PREFIXES:
        if path == prefix or path.startswith(prefix + "/") or path.startswith(prefix + "?"):
            return prefix
    return None


def handle(handler, path, body):
    prefix = _match_prefix(path)
    if prefix is None:
        handler._json({"error": "no sandbox proxy mapping", "path": path}, status=404)
        return

    rest = path[len(prefix):]
    query = urlparse(handler.path).query
    url = UPSTREAM_BASE + prefix + rest
    if query:
        url += "?" + query

    fwd_headers = {k: v for k, v in handler.headers.items() if k.lower() not in _HOP_BY_HOP}
    data = body if body else None
    req = urllib.request.Request(url, data=data, method=handler.command, headers=fwd_headers)

    try:
        with urllib.request.urlopen(req, timeout=30) as resp:
            status = resp.status
            resp_body = resp.read()
            content_type = resp.headers.get("Content-Type", "application/json")
    except urllib.error.HTTPError as e:
        status = e.code
        resp_body = e.read()
        content_type = e.headers.get("Content-Type", "application/json")
    except Exception as e:
        handler._json({"error": "sandbox proxy failed", "upstream": url, "detail": str(e)}, status=502)
        return

    handler._raw(status, content_type, resp_body)
