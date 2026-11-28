"""Gridline (Aadhaar/bank account verification) mock."""

import json
import logging
import threading
import uuid
from datetime import datetime, timezone
from urllib.parse import urlparse, parse_qs
from status_store import extract_path_ids, deep_merge

from ._env import DRIVER_APP_PORT

log = logging.getLogger("gridline")

# Callbacks go to this stack's driver app. Under per-workspace port remapping (devbox,
# resolve-ports) DRIVER_APP_PORT is not 8016; a hard-coded 8016 would deliver the result to
# another workspace's driver app (or nowhere) and the document would stay pending.
WEBHOOK_URL = f"http://localhost:{DRIVER_APP_PORT}/service/idfy/verification"
WEBHOOK_SECRET = "test-secret"
CALLBACK_DELAY = 0.5

# Static values returned by the extract/verify mock — must match what postman env sends.
_MOCK_PAN_NUMBER = "ABCDE1234F"
_MOCK_AADHAAR_NUMBER = "123456789012"

# Court Record Check (CRC) webhook is merchant/city-scoped and uses the
# Verification_Idfy secret. Hardcoded for the local NY Bangalore test env
# (the async submit body carries only group_id=driverId, not merchant/city).
# Routed via caddy (8016), which proxies the webhook path to the driver-app.
CRC_WEBHOOK_URL = f"http://localhost:{DRIVER_APP_PORT}/NAMMA_YATRI_PARTNER/Bangalore/service/idfy/verification"


def _post_webhook(url, payload, label, request_id):
    """POST a verification result to the driver app the way Idfy does (secret in Authorization)."""
    import urllib.request
    req = urllib.request.Request(
        url,
        data=json.dumps(payload).encode(),
        headers={"Content-Type": "application/json", "Authorization": WEBHOOK_SECRET},
        method="POST",
    )
    try:
        with urllib.request.urlopen(req, timeout=5) as resp:
            log.info(f"{label} callback sent for {request_id}: {resp.status}")
    except Exception as e:
        log.error(f"{label} callback failed for {request_id}: {e}")


def _send_dl_callback(request_id, group_id, task_id, dl_number, dob):
    """DL result, shaped like IdfyResponse (SourceOutput DLVerificationOutput) in shared-kernel.

    The driver app finds the upload by request_id (the one in the ack), reads the expiry with
    convertTextToUTC (%Y-%m-%d) and needs one class of vehicle the merchant allows
    (validateDLStatus), so the result lists car, auto, commercial and bike classes.
    """
    import time
    time.sleep(CALLBACK_DELAY)
    now = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.%fZ")
    payload = {
        "action": "verify_with_source",
        "completed_at": now,
        "created_at": now,
        "group_id": group_id,
        "request_id": request_id,
        "result": {
            "source_output": {
                "id_number": dl_number,
                "dob": dob,
                "name": "TEST DRIVER",
                "status": "id_found",
                "dl_status": "Active",
                "source": "SARATHI",
                "date_of_issue": "2015-01-01",
                "t_validity_to": "2040-12-31",
                "nt_validity_to": "2040-12-31",
                "cov_details": [
                    {"cov": "LMV", "category": "NT", "issue_date": "2015-01-01"},
                    {"cov": "3WT", "category": "NT", "issue_date": "2015-01-01"},
                    {"cov": "TRANS", "category": "TR", "issue_date": "2015-01-01"},
                    {"cov": "MCWG", "category": "NT", "issue_date": "2015-01-01"},
                ],
            },
        },
        "status": "completed",
        "task_id": task_id,
        "type": "ind_driving_license",
    }
    _post_webhook(WEBHOOK_URL, payload, "DL", request_id)


def _send_crc_callback(request_id, group_id, task_id):
    """Send a Court Record Check webhook callback to BPP after a delay."""
    import time
    time.sleep(CALLBACK_DELAY)
    now = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.%fZ")
    payload = {
        "action": "court_record_check",
        "completed_at": now,
        "created_at": now,
        "group_id": group_id,
        "request_id": request_id,
        "result": {
            "source_output": {
                "case_details_link": "https://example.com/cases/" + group_id,
                "number_of_cases": 0,
                "report_download_link": "https://example.com/report/" + group_id,
                "risk_summary": "No pending cases found",
                "risk_type": "low",
                "status": "completed",
            },
        },
        "status": "completed",
        "task_id": task_id,
        "type": "ind_court_record",
    }
    _post_webhook(CRC_WEBHOOK_URL, payload, "CRC", request_id)


def _send_pan_callback(request_id, group_id, task_id, pan_number):
    """Send a PAN verification webhook callback to BPP after a delay."""
    import time
    time.sleep(CALLBACK_DELAY)
    now = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.%fZ")
    payload = {
        "action": "verify_with_source",
        "completed_at": now,
        "created_at": now,
        "group_id": group_id,
        "request_id": request_id,
        "result": {
            "source_output": {
                "aadhaar_seeding_status": True,
                "pan_status": "VALID",
                "name_match": True,
                "dob_match": True,
                "status": "id_found",
                "input_details": {
                    "input_pan_number": pan_number,
                    "input_full_name": "TEST DRIVER",
                    "input_dob": "1990-01-01",
                },
            },
        },
        "status": "completed",
        "task_id": task_id,
        "type": "ind_pan",
    }
    _post_webhook(WEBHOOK_URL, payload, "PAN", request_id)


def _send_rc_callback(request_id, group_id, task_id, rc_number):
    """Send an RC verification webhook callback to BPP after a delay."""
    import time
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
    _post_webhook(WEBHOOK_URL, payload, "RC", request_id)


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

        parsed_body = {}
        if body:
            try:
                text = body.decode("utf-8") if isinstance(body, bytes) else body
                parsed_body = json.loads(text)
            except (json.JSONDecodeError, ValueError):
                pass
        if not isinstance(parsed_body, dict):
            parsed_body = {}
        group_id = parsed_body.get("group_id", "mock-group")
        task_id = parsed_body.get("task_id", "mock-task")

        # For DL verification, send a webhook callback after a delay
        if "ind_driving_license" in path:
            dl_data = parsed_body.get("data") or {}
            threading.Thread(
                target=_send_dl_callback,
                args=(request_id, group_id, task_id, dl_data.get("id_number", "UNKNOWN"), dl_data.get("date_of_birth")),
                daemon=True,
            ).start()

        # For PAN verification, send a webhook callback after a delay
        if "ind_pan" in path:
            pan_number = ((parsed_body.get("data") or {}).get("id_number")
                          or (parsed_body.get("data") or {}).get("pan_number")
                          or _MOCK_PAN_NUMBER)
            threading.Thread(
                target=_send_pan_callback,
                args=(request_id, group_id, task_id, pan_number),
                daemon=True,
            ).start()

        # For RC verification, send a webhook callback after a delay
        if "ind_rc" in path:
            rc_number = (parsed_body.get("data") or {}).get("rc_number", "UNKNOWN")
            threading.Thread(
                target=_send_rc_callback,
                args=(request_id, group_id, task_id, rc_number),
                daemon=True,
            ).start()

        # For Court Record Check, send a CRC webhook callback after a delay
        if "ind_court_record" in path:
            threading.Thread(
                target=_send_crc_callback,
                args=(request_id, group_id, task_id),
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
