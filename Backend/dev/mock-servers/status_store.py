"""In-memory status store + request-matching override system.

Two mechanisms:

1. **Status store** (existing): keyed by (service, id)
   POST /mock/status {"service": "juspay", "id": ["ord-123"], "status": "CHARGED"}

2. **Override rules** (new): match incoming requests by extracting IDs from path/query/body
   POST /mock/override {
     "service": "cmrl",
     "extract": "body.mob",          // where to find the ID in incoming requests
     "value": "9876543210",           // what value to match
     "response": {"returnCode": "500", "returnMessage": "Technical Failure"}
   }

   Extract syntax:
     body.<json_path>    — from JSON request body (supports nested: body.data.customer.phone)
     path.<index>        — from URL path segment by index (0-based)
     query.<param>       — from query parameter
     header.<name>       — from request header

   When a request to the matching service has the extracted value == value,
   the "response" dict is deep-merged into the handler's default response.
"""

import json
import logging
import threading
import time

log = logging.getLogger("mock-server")

STATUS_TTL = 300  # 5 minutes

# ── Status store (existing) ──

_store = {}
_extract_store = {}  # (service) → [{extract, status, data, expires}]
_lock = threading.Lock()


def set_status(service, identifiers, status, data=None):
    if isinstance(identifiers, str):
        identifiers = [identifiers]
    entry = {"status": status, "data": data or {}, "expires": time.time() + STATUS_TTL}
    with _lock:
        for ident in identifiers:
            _store[(service, ident)] = entry
    log.info(f"Status set: {service}/[{', '.join(identifiers)}] = {status}")
    return len(identifiers)


def get_status(service, identifier):
    key = (service, identifier)
    with _lock:
        entry = _store.get(key)
        if entry is None:
            return None
        if time.time() > entry["expires"]:
            del _store[key]
            return None
        return entry


def set_extract_status(service, extract, status, match=None, data=None):
    """Store a status rule that matches future requests by extracting an ID.

    Args:
        service: service name (e.g. "juspay")
        extract: where to find the ID (e.g. "path.2")
        status: status to return when matched
        match: API path pattern to match (e.g. "/orders" matches any path containing /orders).
               If None, matches all requests to the service.
        data: extra data to deep-merge into response
    """
    entry = {"extract": extract, "status": status, "match": match,
             "data": data or {}, "expires": time.time() + STATUS_TTL}
    with _lock:
        _extract_store.setdefault(service, []).append(entry)
    match_desc = f" on {match}" if match else ""
    log.info(f"Extract status set: {service}{match_desc} extract={extract} → {status}")


def get_extract_status(service, path, body=None, query=None, headers=None):
    """Check if any extract-based status rule matches this request.
    Returns (status, data) or (None, {})."""
    with _lock:
        rules = _extract_store.get(service, [])
    now = time.time()
    for rule in rules:
        if now > rule["expires"]:
            continue
        # Check path pattern match
        if rule.get("match") and rule["match"] not in path:
            continue
        extract = rule["extract"]
        extracted_id = _extract_value(extract, path, body, query, headers)
        if extracted_id:
            # Match: store as explicit ID for future lookups too
            set_status(service, [extracted_id], rule["status"], rule["data"])
            return rule["status"], rule["data"]
    return None, {}


def _extract_value(extract, path, body=None, query=None, headers=None):
    """Extract a value from a request using the extract syntax."""
    if extract.startswith("path."):
        idx = int(extract.split(".", 1)[1])
        parts = [p for p in path.strip("/").split("/") if p]
        return parts[idx] if idx < len(parts) else None
    elif extract.startswith("body."):
        json_path = extract.split(".", 1)[1]
        if body and isinstance(body, dict):
            val = body
            for key in json_path.split("."):
                if isinstance(val, dict):
                    val = val.get(key)
                else:
                    return None
            return str(val) if val is not None else None
    elif extract.startswith("query."):
        param = extract.split(".", 1)[1]
        if query and isinstance(query, dict):
            return query.get(param)
    elif extract.startswith("header."):
        name = extract.split(".", 1)[1]
        if headers and isinstance(headers, dict):
            return headers.get(name)
    return None


def list_statuses():
    now = time.time()
    with _lock:
        for k in [k for k, v in _store.items() if now > v["expires"]]:
            del _store[k]
        return {
            f"{k[0]}/{k[1]}": {"status": v["status"], "data": v["data"], "ttl": int(v["expires"] - now)}
            for k, v in _store.items()
        }


def clear_statuses():
    with _lock:
        _store.clear()


# ── Override rules (new) ──

_overrides = []  # list of {service, extract, value, response, expires}
_override_lock = threading.Lock()


def add_override(service, extract, value, response):
    """Add an override rule. When a request to `service` has `extract` == `value`,
    merge `response` into the handler's default response."""
    entry = {
        "service": service,
        "extract": extract,
        "value": str(value),
        "response": response or {},
        "expires": time.time() + STATUS_TTL,
    }
    with _override_lock:
        _overrides.append(entry)
    log.info(f"Override added: {service} where {extract}={value}")


def list_overrides():
    now = time.time()
    with _override_lock:
        _overrides[:] = [o for o in _overrides if o["expires"] > now]
        return [
            {"service": o["service"], "extract": o["extract"], "value": o["value"],
             "response": o["response"], "ttl": int(o["expires"] - now)}
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

    # Try body decoder first (handles encrypted bodies like CRIS/CMRL),
    # then fall back to the plain-parsed body
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
            extracted = _extract_value(o["extract"], path, query_string, headers, decoded_body)
            if extracted is not None and str(extracted) == o["value"]:
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


# ── Shared utilities ──

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
