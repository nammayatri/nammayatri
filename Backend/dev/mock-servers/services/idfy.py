"""Mock Idfy — replaces the Haskell mock-idfy-exe (port 6235).

Routes (mounted under any path containing "/idfy"):
  POST  /idfy/v3/tasks/async/verify_with_source/ind_dl
  POST  /idfy/v3/tasks/async/verify_with_source/ind_rc
  POST  /idfy/v3/tasks/async/verify_with_source/ind_pan
  POST  /idfy/v3/tasks/async/verify_with_source/ind_gst_certificate
  POST  /idfy/v3/tasks/async/verify_with_source/ind_pan_aadhaar_link
        — all five return `{request_id, _a: null}` immediately. The Haskell mock
        also POSTs a webhook callback after 500ms; replicated optionally below
        (only fires if `IDFY_WEBHOOK_URL` env var is set, since most existing
        tests bypass the callback via `check_extraction=false`).

  POST  /idfy/v3/tasks/async/validate_image
  POST  /idfy/v3/tasks/async/extract_image/ind_dl
  POST  /idfy/v3/tasks/async/extract_image/ind_rc
  POST  /idfy/v3/tasks/async/extract_image/ind_pan
  POST  /idfy/v3/tasks/async/extract_image/ind_gst
  POST  /idfy/v3/tasks/async/extract_image/ind_aadhaar
  POST  /idfy/v3/tasks/async/compare
        — wrap their result in an IdfyResponse envelope.
  POST  /idfy/v3/tasks/sync/compare/face
        — selfie-vs-document face match; returns `{result: {is_a_match, match_score, ...}}`.
          Defaults to a match; force a mismatch via POST /mock/override (is_a_match=false).

  POST  /idfy/configure                — update the in-memory MockDocConfig used
                                         by the extract endpoints.

The `_get_override` hook still works on this module: an explicit
`POST /mock/override {"service": "idfy", "extract": "...", "value": "...", "response": {...}}`
overrides the response payload for any verify/extract call whose request matches.
"""

import json
import os
import threading
import uuid
from datetime import datetime, timezone

import status_store

_LOCK = threading.Lock()
_CONFIG = {
    "panNumber": "ABCDE1234F",
    "gstNumber": "29ABCDE1234F1Z5",
    "aadhaarNumber": "123456789012",
    "dlNumber": "KA0120200012345",
}
_WEBHOOK_URL = os.environ.get("IDFY_WEBHOOK_URL", "")
_WEBHOOK_DELAY_MS = int(os.environ.get("IDFY_WEBHOOK_DELAY_MS", "500"))


def _now_iso():
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.") + f"{datetime.now(timezone.utc).microsecond:06d}Z"


def _ack(body_json):
    """Verification endpoints return this shape; the webhook fires asynchronously."""
    req_id = str(uuid.uuid4())
    return {"request_id": req_id, "_a": None}


def _idfy_envelope(result):
    """Wrap an extract/validate result in the IdfyResponse envelope the Haskell mock used."""
    now = _now_iso()
    return {
        "action": "action",
        "completed_at": now,
        "created_at": now,
        "group_id": "gid",
        "request_id": str(uuid.uuid4()),
        "result": result,
        "status": "status",
        "task_id": "task_id",
        "type": "type",
    }


def _fire_webhook(tag, payload):
    """Optional: POST the verification result to a configured webhook URL."""
    if not _WEBHOOK_URL:
        return

    def _do_send():
        import urllib.request
        try:
            req = urllib.request.Request(
                _WEBHOOK_URL,
                data=json.dumps({"tag": tag, "data": payload}).encode("utf-8"),
                headers={"Content-Type": "application/json"},
            )
            urllib.request.urlopen(req, timeout=5).read()
        except Exception:
            pass

    threading.Timer(_WEBHOOK_DELAY_MS / 1000.0, _do_send).start()


def _dual(d):
    # Different shared-kernel pins parse Idfy extract under different wrappers:
    # older flows read `source_output`, c72fffc reads `extraction_output` (ExtractionOutput a).
    # Emit both (Aeson ignores the unused key) so the mock works regardless of pin.
    return {"source_output": d, "extraction_output": d}


def _dl_extract():
    with _LOCK:
        cfg = dict(_CONFIG)
    return _dual({
        "id_number": cfg["dlNumber"], "name_on_card": "name_on_card", "fathers_name": "fathers_name",
        "date_of_birth": "date_of_birth", "date_of_validity": "date_of_validity",
        "address": "address", "district": "district", "street_address": "street_address",
        "pincode": "pincode", "state": "state", "issue_dates": None,
        "type": ["W_CAB"], "validity": None, "status": "status",
    })


def _rc_extract():
    return _dual({
        "address": "address", "body": "body", "chassis_number": "chassis_number",
        "class": "_class", "colour": "colour", "cubic_capacity": "cubic_capacity",
        "document1_side": "document1_side", "document2_side": "document2_side",
        "engine_number": "engine_number", "fathers_name": "fathers_name",
        "fuel": "fuel", "manufacturer": "manufacturer",
        "manufacturing_date": "manufacturing_date", "model": "model",
        "owner_name": "owner_name", "registration_date": "registration_date",
        "registration_number": "registration_number", "rto_district": "rto_district",
        "state": "state", "wheel_base": "wheel_base", "status": "status",
    })


def _pan_extract():
    with _LOCK:
        cfg = dict(_CONFIG)
    return _dual({
        "age": None, "date_of_birth": "1990-01-01", "date_of_issue": None,
        "fathers_name": "FATHER NAME", "id_number": cfg["panNumber"],
        "is_scanned": True, "minor": False, "name_on_card": "TEST FLEET OWNER",
        "pan_type": "Individual",
    })


def _gst_extract():
    with _LOCK:
        cfg = dict(_CONFIG)
    return _dual({
        "address": "123 Test Street",
        "constitution_of_business": "Private Limited Company",
        "date_of_liability": "2020-01-01",
        "gstin": cfg["gstNumber"],
        "is_provisional": False,
        "legal_name": "TEST FLEET PVT LTD",
        "pan_number": cfg["panNumber"],
        "trade_name": "TEST FLEET",
        "type_of_registration": "Regular",
        "valid_from": "2020-01-01",
        "valid_upto": None,
    })


def _aadhaar_extract():
    with _LOCK:
        cfg = dict(_CONFIG)
    return {
        "extraction_output": {
            "address": "123 Test Street, Delhi",
            "date_of_birth": "1990-01-01",
            "district": "Delhi",
            "fathers_name": "FATHER NAME",
            "gender": "MALE",
            "house_number": "123",
            "id_number": cfg["aadhaarNumber"],
            "is_scanned": True,
            "name_on_card": "TEST FLEET OWNER",
            "pincode": "110001",
            "state": "Delhi",
            "street_address": "Test Street",
            "year_of_birth": "1990",
        },
        "qr_output": {
            "address": None, "date_of_birth": None, "district": None,
            "gender": None, "house_number": None, "id_number": None,
            "name_on_card": None, "pincode": None, "state": None,
            "street_address": None, "year_of_birth": None,
        },
    }


def _validate_image(body_json):
    doc_type = "doc_type"
    if isinstance(body_json, dict):
        data = body_json.get("data") or {}
        if isinstance(data, dict):
            doc_type = data.get("doc_type", doc_type)
    return {
        "detected_doc_type": doc_type,
        "is_readable": True,
        "readability": {"confidence": 70, "dummyField": None},
    }


def _name_compare():
    return {"match_output": {"name_match": 100}}


def _face_compare(is_match=True):
    """IDfy POST /v3/tasks/sync/compare/face result. Default is a match;
    tests force a mismatch via POST /mock/override (service=idfy) returning is_a_match=false."""
    return {
        "image_1": None,
        "image_2": None,
        "is_a_match": is_match,
        "match_score": 99.5 if is_match else 11.2,
        "review_recommended": False,
    }


def handle(handler, path, body):
    text = body.decode("utf-8") if isinstance(body, bytes) and body else (body or "")
    try:
        body_json = json.loads(text) if text else {}
    except (json.JSONDecodeError, ValueError):
        body_json = {}

    # Allow /mock/override rules to short-circuit with a fully synthetic response.
    # Face-compare is handled below instead, so it can emit a valid IdfyResponse envelope with the
    # overridden is_a_match (a raw replacement drops required envelope fields like status/task_id).
    override_status, override_data = handler._get_override("idfy")
    if override_status and isinstance(override_data, dict) and "/compare/face" not in path:
        return handler._json(override_data)

    if path.endswith("/configure"):
        if handler.command == "POST" and isinstance(body_json, dict):
            with _LOCK:
                for k in ("panNumber", "gstNumber", "aadhaarNumber"):
                    if k in body_json:
                        _CONFIG[k] = body_json[k]
                return handler._json(dict(_CONFIG))
        with _LOCK:
            return handler._json(dict(_CONFIG))

    # Verification (async-callback shape)
    if "/verify_with_source/ind_dl" in path or "/verify_with_source/ind_driving_license" in path:
        resp = _ack(body_json)
        _fire_webhook("DLVerificationResult", body_json)
        return handler._json(resp)
    if "/verify_with_source/ind_rc" in path:
        resp = _ack(body_json)
        _fire_webhook("RCVerificationResult", body_json)
        return handler._json(resp)
    if "/verify_with_source/ind_pan_aadhaar_link" in path:
        resp = _ack(body_json)
        _fire_webhook("PanVerificationResult", body_json)
        return handler._json(resp)
    if "/verify_with_source/ind_pan" in path:
        resp = _ack(body_json)
        _fire_webhook("PanVerificationResult", body_json)
        return handler._json(resp)
    if "/verify_with_source/ind_gst" in path:
        resp = _ack(body_json)
        _fire_webhook("GstVerificationResult", body_json)
        return handler._json(resp)

    # Validate / extract (synchronous) — match both /extract_image/ind_X and /extract/ind_X path styles.
    if "/validate_image" in path or "/validate/" in path:
        return handler._json(_idfy_envelope(_validate_image(body_json)))
    if "extract" in path:
        if "ind_dl" in path or "driving_license" in path:
            return handler._json(_idfy_envelope(_dl_extract()))
        if "ind_rc" in path:
            return handler._json(_idfy_envelope(_rc_extract()))
        if "ind_pan" in path:
            return handler._json(_idfy_envelope(_pan_extract()))
        if "ind_gst" in path:
            return handler._json(_idfy_envelope(_gst_extract()))
        if "ind_aadhaar" in path:
            return handler._json(_idfy_envelope(_aadhaar_extract()))
    if "/compare/face" in path:
        # A /mock/override (service=gridline, match=/compare/face) can force the outcome by carrying
        # a `result` with the desired is_a_match (true / false / null). Re-wrap it in a valid
        # IdfyResponse envelope so the BE parses it; absent an override, default to a match.
        if isinstance(override_data, dict) and isinstance(override_data.get("result"), dict):
            return handler._json(_idfy_envelope(override_data["result"]))
        return handler._json(_idfy_envelope(_face_compare(True)))
    if "/compare" in path:
        return handler._json(_idfy_envelope(_name_compare()))

    # Fallback: empty IdfyResponse envelope, useful for unknown verify endpoints.
    return handler._json(_idfy_envelope(None))
