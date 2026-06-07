"""Request-matching override system for the mock server.

POST /mock/override registers a rule that fires when a future request to the
named service has an extracted field equal to `value`. The rule's `response`
dict is then deep-merged into the handler's default response.

  POST /mock/override {
    "service": "juspay",
    "extract": "path.2",                  // where to find the value in the request
    "value": "ord_abc123",                 // exact match required
    "match": "/orders",                    // optional path-substring gate
    "response": {"status": "CHARGED", "amount": 10.0}
  }

  Extract syntax:
    body.<json_path>    from JSON request body (supports nested: body.data.customer.phone)
    path.<index>        from URL path segment by index (0-based)
    query.<param>       from query parameter
    header.<name>       from request header
"""

import logging
import threading
import time

log = logging.getLogger("mock-server")

STATUS_TTL = 300  # 5 minutes

_overrides = []  # list of {service, extract, value, match, response, expires}
_override_lock = threading.Lock()


def add_override(service, extract, value, response, match=None):
    """Register an override rule. When a request to `service` has the extracted
    field equal to `value` (and the path contains `match`, if provided),
    `response` is deep-merged into the handler's default response."""
    entry = {
        "service": service,
        "extract": extract,
        "value": str(value),
        "match": match,
        "response": response or {},
        "expires": time.time() + STATUS_TTL,
    }
    with _override_lock:
        _overrides.append(entry)
    match_desc = f" on {match}" if match else ""
    log.info(f"Override added: {service}{match_desc} where {extract}={value}")


def list_overrides():
    now = time.time()
    with _override_lock:
        _overrides[:] = [o for o in _overrides if o["expires"] > now]
        return [
            {"service": o["service"], "extract": o["extract"], "value": o["value"],
             "match": o.get("match"), "response": o["response"], "ttl": int(o["expires"] - now)}
            for o in _overrides
        ]


def clear_overrides():
    with _override_lock:
        _overrides.clear()


def delete_override(service, extract, value):
    """Delete a specific override matching service + extract + value."""
    removed = 0
    with _override_lock:
        before = len(_overrides)
        _overrides[:] = [
            o for o in _overrides
            if not (o["service"] == service and o["extract"] == extract and o["value"] == str(value))
        ]
        removed = before - len(_overrides)
    if removed:
        log.info(f"Override deleted: {service} {extract}={value}")
    return removed


_body_decoders = {}  # service -> function(body_raw) -> dict


def register_body_decoder(service, decoder_fn):
    """Register a function that decodes/decrypts request bodies for a service.
    The decoder takes raw body bytes and returns a parsed dict.
    Used by the override middleware to extract fields from encrypted requests.
    """
    _body_decoders[service] = decoder_fn


def check_overrides(service, path, query_string, headers, body_parsed, body_raw=None):
    """Check all active overrides for a service against the current request.
    Returns the merged response dict if any override matches, else {}.
    """
    now = time.time()
    merged = {}

    decoded_body = None
    if body_raw and service in _body_decoders:
        try:
            decoded_body = _body_decoders[service](body_raw)
        except Exception:
            pass
    if not decoded_body:
        decoded_body = body_parsed

    with _override_lock:
        _overrides[:] = [o for o in _overrides if o["expires"] > now]
        for o in _overrides:
            if o["service"] != service:
                continue
            if o.get("match") and o["match"] not in path:
                continue
            extracted = _extract_value(o["extract"], path, query_string, headers, decoded_body)
            if extracted is not None:
                matches = o["value"] in str(extracted) if o["extract"] == "path" else str(extracted) == o["value"]
                if matches:
                    merged = deep_merge(merged, o["response"])
                    log.info(f"Override matched: {service} {o['extract']}={o['value']}")
    return merged


def _extract_value(extract_path, url_path, query_string, headers, body_parsed):
    """Extract a value from the request using the extract path syntax."""
    parts = extract_path.split(".", 1)
    source = parts[0]
    field = parts[1] if len(parts) > 1 else None

    if source == "body" and field and body_parsed:
        return _jq_get(body_parsed, field)
    elif source == "path" and field:
        try:
            idx = int(field)
            segments = [s for s in url_path.strip("/").split("/") if s]
            return segments[idx] if idx < len(segments) else None
        except (ValueError, IndexError):
            return None
    elif source == "path" and not field:
        return url_path
    elif source == "query" and field and query_string:
        from urllib.parse import parse_qs
        params = parse_qs(query_string)
        vals = params.get(field)
        return vals[0] if vals else None
    elif source == "header" and field and headers:
        return headers.get(field) or headers.get(field.lower())
    return None


def _jq_get(obj, path):
    """Simple jq-like nested field access: 'data.customer.phone' → obj['data']['customer']['phone']"""
    for key in path.split("."):
        if isinstance(obj, dict):
            obj = obj.get(key)
        else:
            return None
    return obj


def deep_merge(base, override):
    """Merge override into base. Override wins for scalars; dicts merge recursively."""
    result = dict(base)
    for k, v in override.items():
        if k in result and isinstance(result[k], dict) and isinstance(v, dict):
            result[k] = deep_merge(result[k], v)
        else:
            result[k] = v
    return result


def extract_path_ids(path):
    skip = {"v1", "v2", "api", "service", "mock", "status", "health", "override"}
    return [p for p in path.strip("/").split("/") if p and p.lower() not in skip and not p.startswith("{")]
