"""Gridline (Aadhaar/bank account verification) mock."""

import json
import logging
import threading
import uuid
from datetime import datetime, timezone
from urllib.parse import urlparse, parse_qs
from status_store import extract_path_ids, deep_merge

log = logging.getLogger("gridline")

WEBHOOK_URL = "http://localhost:8016/service/idfy/verification"
WEBHOOK_SECRET = "xxxxxxx"
CALLBACK_DELAY = 0.5


def _send_rc_callback(request_id, group_id, task_id, rc_number):
    """Send an RC verification webhook callback to BPP after a delay."""
    import time
    import urllib.request
    time.sleep(CALLBACK_DELAY)
    now = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.%fZ")
    payload = {
        "action": "verify_with_source",
        "completed_at": now,
        "created_at": now,
        "group_id": group_id,
        "request_id": request_id,
        "result": {
            "source_output": None,
            "extraction_output": {
                "registration_number": rc_number,
                "vehicle_class": "3WT_CAB",
                "status": "id_found",
                "fitness_upto": "2036-12-27",
                "fuel_type": "PETROL",
                "registration_date": "2021-12-28",
                "owner_name": "TEST DRIVER",
                "manufacturer": "TOYOTA",
                "manufacturer_model": "INNOVA",
                "insurance_validity": "3026-12-21",
                "chassis_number": "MB8DP12DMM89XXXXX",
                "engine_number": "AF2127XXXXX",
                "colour": "White",
                "color": "White",
                "status_message": "RC is Active",
                "seating_capacity": "7",
            },
        },
        "status": "completed",
        "task_id": task_id,
        "type": "ind_rc",
    }
    body = json.dumps(payload).encode()
    req = urllib.request.Request(
        WEBHOOK_URL,
        data=body,
        headers={"Content-Type": "application/json", "Authorization": WEBHOOK_SECRET},
        method="POST",
    )
    try:
        with urllib.request.urlopen(req, timeout=5) as resp:
            log.info(f"RC callback sent for {request_id}: {resp.status}")
    except Exception as e:
        log.error(f"RC callback failed for {request_id}: {e}")


def handle(handler, path, body):
    # Document-onboarding Idfy endpoints (image extract/validate + selfie face-compare) need real
    # payloads, not gridline's generic ack/verified fallback — delegate them to the idfy mock so the
    # PAN/Aadhaar onboarding + face-match flow works when Verification_Idfy is routed to /gridline.
    if any(s in path for s in ("/extract_image", "/extract/", "/validate_image", "/validate/", "/compare")):
        from services import idfy
        return idfy.handle(handler, path, body)
    path_ids = extract_path_ids(path)
    override_status, extra = handler._get_override("gridline", *path_ids)

    # Idfy async verify endpoints (POST .../tasks/async/...) return IdfySuccess
    if "tasks/async" in path:
        request_id = str(uuid.uuid4())
        base = {
            "request_id": request_id,
        }
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)

        # For RC verification, send a webhook callback after a delay
        if "ind_rc" in path:
            parsed_body = {}
            if body:
                try:
                    text = body.decode("utf-8") if isinstance(body, bytes) else body
                    parsed_body = json.loads(text)
                except (json.JSONDecodeError, ValueError):
                    pass
            rc_number = (parsed_body.get("data") or {}).get("rc_number", "UNKNOWN")
            group_id = parsed_body.get("group_id", "mock-group")
            task_id = parsed_body.get("task_id", "mock-task")
            threading.Thread(
                target=_send_rc_callback,
                args=(request_id, group_id, task_id, rc_number),
                daemon=True,
            ).start()
        return

    # Idfy getTask endpoint (GET /v3/tasks?request_id=...) — polls verification status
    if "/v3/tasks" in path and handler.command == "GET":
        parsed = urlparse(handler.path)
        qs = parse_qs(parsed.query)
        request_id = qs.get("request_id", [str(uuid.uuid4())])[0]
        now = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.000Z")

        # Determine type from the last async request or default to bank account
        doc_type = "validate_bank_account"
        base = {
            "action": doc_type,
            "completed_at": now,
            "created_at": now,
            "group_id": "mock-group",
            "request_id": request_id,
            "result": {
                "account_exists": "YES",
                "amount_deposited": None,
                "bank_account_number": "1234567890",
                "ifsc_code": "HDFC0001234",
                "message": "Bank account verified",
                "name_at_bank": "Test Driver",
                "status": "completed",
            },
            "status": "completed",
            "task_id": "mock-task-" + str(uuid.uuid4())[:8],
            "type": doc_type,
        }
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)
        return

    base = {
        "status": override_status or "success",
        "data": {"verified": True},
    }
    if extra:
        base = deep_merge(base, extra)
    handler._json(base)
