"""HyperVerge verification mock."""

import json
import uuid

from status_store import extract_path_ids, deep_merge


def _parse_body(body):
    try:
        text = body.decode("utf-8") if isinstance(body, bytes) and body else (body or "")
        return json.loads(text) if text else {}
    except (json.JSONDecodeError, ValueError, AttributeError):
        return {}


def handle(handler, path, body):
    path_ids = extract_path_ids(path)
    override_status, extra = handler._get_override("hyperverge", *path_ids)

    # SDK on-device verification result (POST /v1/output), consumed by checkIfGenuineReq ->
    # verifySdkResp. The driver app submits a ProfilePhoto captured via the HyperVerge SDK; the BE
    # calls /v1/output with {transactionId} and requires: statusCode 200, an echoed transactionId,
    # a status that maps to the submitted validationStatus (auto_approved -> AUTO_APPROVED -> VALID),
    # and a SelfieFlow userDetails ({selfieURL}). Default to a passing selfie; a test can force a
    # different outcome via /mock/override (e.g. {"result": {"status": "needs_review"}}).
    if "/v1/output" in path or path.endswith("/output"):
        body_json = _parse_body(body)
        txn = body_json.get("transactionId") if isinstance(body_json, dict) else None
        base = {
            "status": "success",
            "statusCode": 200,
            "metadata": {"requestId": str(uuid.uuid4())},
            "result": {
                "status": "auto_approved",
                "transactionId": txn,
                "userDetails": {"selfieURL": "https://mock.local/selfie.jpg"},
                "flags": None,
                "failureReason": None,
                "error": None,
            },
        }
        if extra:
            base = deep_merge(base, extra)
        return handler._json(base)

    base = {
        "status": override_status or "success",
        "result": {"verified": True},
    }
    if extra:
        base = deep_merge(base, extra)
    handler._json(base)
