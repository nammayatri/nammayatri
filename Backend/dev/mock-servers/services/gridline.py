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
WEBHOOK_SECRET = "test-secret"
CALLBACK_DELAY = 0.5

# Static values returned by the extract/verify mock — must match what postman env sends.
_MOCK_PAN_NUMBER = "ABCDE1234F"
_MOCK_AADHAAR_NUMBER = "123456789012"

# Court Record Check (CRC) webhook is merchant/city-scoped and uses the
# Verification_Idfy secret. Hardcoded for the local NY Bangalore test env
# (the async submit body carries only group_id=driverId, not merchant/city).
# Routed via caddy (8016), which proxies the webhook path to the driver-app.
CRC_WEBHOOK_URL = "http://localhost:8016/NAMMA_YATRI_PARTNER/Bangalore/service/idfy/verification"
CRC_WEBHOOK_SECRET = "test-secret"


def _send_crc_callback(request_id, group_id, task_id):
    """Send a Court Record Check webhook callback to BPP after a delay."""
    import time
    import urllib.request
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
    body = json.dumps(payload).encode()
    req = urllib.request.Request(
        CRC_WEBHOOK_URL,
        data=body,
        headers={"Content-Type": "application/json", "Authorization": CRC_WEBHOOK_SECRET},
        method="POST",
    )
    try:
        with urllib.request.urlopen(req, timeout=5) as resp:
            log.info(f"CRC callback sent for {request_id} (driver {group_id}): {resp.status}")
    except Exception as e:
        log.error(f"CRC callback failed for {request_id}: {e}")


def _send_pan_callback(request_id, group_id, task_id, pan_number):
    """Send a PAN verification webhook callback to BPP after a delay."""
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
    body = json.dumps(payload).encode()
    req = urllib.request.Request(
        WEBHOOK_URL,
        data=body,
        headers={"Content-Type": "application/json", "Authorization": WEBHOOK_SECRET},
        method="POST",
    )
    try:
        with urllib.request.urlopen(req, timeout=5) as resp:
            log.info(f"PAN callback sent for {request_id}: {resp.status}")
    except Exception as e:
        log.error(f"PAN callback failed for {request_id}: {e}")


def _send_rc_callback(request_id, group_id, task_id, rc_number, override=None):
    """Send an RC verification webhook callback to BPP after a delay.

    WHY override: every other gridline path already honours the per-test _get_override payload;
    the RC callback ignored it. COMPAT: override=None leaves the payload exactly as before.
    """
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
    if override:
        payload = deep_merge(payload, override)
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


def _send_dl_callback(request_id, group_id, task_id, dl_number):
    """Send a DL verification webhook callback to BPP after a delay.

    WHY: gridline used to answer DL verification with a bare ack, so the DL never resolved and a
    driver could not finish onboarding. COV `LMV` is in the Delhi/MSIL DriverLicense
    supported-classes list and nt_validity_to is far future, so validateDLStatus marks it VALID.

    NOTE: unlike the GST callback this is NOT behind a flag, so any merchant routed to Gridline for
    DL now gets an auto-VALID licence where it previously got nothing. Nothing does today (MSIL uses
    Idfy for DL), but gate it on a config flag if that changes. The dob below is fixed, so a test
    using a different DOB will fail the Aadhaar/DL cross-check.
    """
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
            "source_output": {
                "address": "Address as available on Gov. Source",
                "badge_details": None,
                "card_serial_no": None,
                "city": "Delhi",
                "cov_details": [
                    {"category": "NT", "cov": "LMV", "issue_date": "2018-01-01"},
                ],
                "date_of_issue": "2018-01-01",
                "date_of_last_transaction": None,
                "dl_status": "Active",
                "dob": "1988-03-12",
                "face_image": None,
                "gender": None,
                "hazardous_valid_till": None,
                "hill_valid_till": None,
                "id_number": dl_number,
                "issuing_rto_name": "DL, DELHI",
                "last_transacted_at": None,
                "name": "TEST DRIVER",
                "nt_validity_from": "2018-01-01",
                "nt_validity_to": "2035-02-14",
                "relatives_name": None,
                "source": "SARATHI",
                "status": "id_found",
                "t_validity_from": None,
                "t_validity_to": None,
            },
        },
        "status": "completed",
        "task_id": task_id,
        "type": "ind_driving_license",
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
            log.info(f"DL callback sent for {request_id}: {resp.status}")
    except Exception as e:
        log.error(f"DL callback failed for {request_id}: {e}")


def _send_gst_callback(request_id, group_id, task_id, gstin):
    """Send a GST verification webhook callback to BPP after a delay.

    WHY: without a callback GST stays MANUAL_VERIFICATION_REQUIRED, so a business fleet can never
    reach verified. onVerifyGstHandler writes VALID only when gstin_status == "Active" AND a
    non-empty legal_name is present for a fleet role — both are supplied below.

    COMPAT: OFF BY DEFAULT, fires only when idfy._CONFIG["gstCallback"] is true. Otherwise
    ind_gst_certificate falls through to the bare ack exactly as before, so PanGstCrossCheckFlow
    (which asserts only the synchronous 400/200 and SQL-sets driver_gstin itself) is unaffected.
    """
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
            "source_output": {
                "additional_place_of_business_fields": None,
                "centre_jurisdiction": "RANGE-I",
                "centre_jurisdiction_code": "XX0101",
                "constitution_of_business": "Private Limited Company",
                "date_of_cancellation": None,
                "date_of_registration": "2020-01-01",
                "gstin": gstin,
                "gstin_status": "Active",
                "last_updated_date": "2020-01-01",
                "legal_name": "TEST FLEET PVT LTD",
                "nature_of_business_activity": None,
                "principal_place_of_business_fields": {
                    "pincode": "110001",
                    "state_name": "Delhi",
                },
                "source": "GSTN",
                "state_jurisdiction_code": "DL01",
                "status": "id_found",
                "taxpayer_type": "Regular",
                "trade_name": "TEST FLEET",
                "e_invoice_status": None,
                "status_details": None,
                "is_sez": False,
                "filing_details": None,
            },
        },
        "status": "completed",
        "task_id": task_id,
        "type": "ind_gst_certificate",
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
            log.info(f"GST callback sent for {request_id}: {resp.status}")
    except Exception as e:
        log.error(f"GST callback failed for {request_id}: {e}")


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

        # For PAN verification, send a webhook callback after a delay
        if "ind_pan" in path:
            parsed_body = {}
            if body:
                try:
                    text = body.decode("utf-8") if isinstance(body, bytes) else body
                    parsed_body = json.loads(text)
                except (json.JSONDecodeError, ValueError):
                    pass
            pan_number = ((parsed_body.get("data") or {}).get("id_number")
                          or (parsed_body.get("data") or {}).get("pan_number")
                          or _MOCK_PAN_NUMBER)
            group_id = parsed_body.get("group_id", "mock-group")
            task_id = parsed_body.get("task_id", "mock-task")
            threading.Thread(
                target=_send_pan_callback,
                args=(request_id, group_id, task_id, pan_number),
                daemon=True,
            ).start()

        # GST callback — opt-in via /idfy/configure {"gstCallback": true}. Default off, so every
        # existing flow keeps the old bare-ack behaviour.
        if "ind_gst" in path:
            from services import idfy as _idfy
            with _idfy._LOCK:
                _enabled = bool(_idfy._CONFIG.get("gstCallback"))
            if _enabled:
                parsed_body = {}
                if body:
                    try:
                        text = body.decode("utf-8") if isinstance(body, bytes) else body
                        parsed_body = json.loads(text)
                    except (json.JSONDecodeError, ValueError):
                        pass
                gstin = ((parsed_body.get("data") or {}).get("gstin")
                         or (parsed_body.get("data") or {}).get("id_number")
                         or _idfy._CONFIG.get("gstNumber"))
                group_id = parsed_body.get("group_id", "mock-group")
                task_id = parsed_body.get("task_id", "mock-task")
                threading.Thread(
                    target=_send_gst_callback,
                    args=(request_id, group_id, task_id, gstin),
                    daemon=True,
                ).start()

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
                args=(request_id, group_id, task_id, rc_number, extra),
                daemon=True,
            ).start()

        # DL callback — resolves the licence to VALID. Always on; see _send_dl_callback for why
        # that is safe today and what to watch for.
        if "ind_dl" in path or "ind_driving_license" in path:
            parsed_body = {}
            if body:
                try:
                    text = body.decode("utf-8") if isinstance(body, bytes) else body
                    parsed_body = json.loads(text)
                except (json.JSONDecodeError, ValueError):
                    pass
            dl_number = (parsed_body.get("data") or {}).get("id_number", "UNKNOWN")
            group_id = parsed_body.get("group_id", "mock-group")
            task_id = parsed_body.get("task_id", "mock-task")
            threading.Thread(
                target=_send_dl_callback,
                args=(request_id, group_id, task_id, dl_number),
                daemon=True,
            ).start()

        # For Court Record Check, send a CRC webhook callback after a delay
        if "ind_court_record" in path:
            parsed_body = {}
            if body:
                try:
                    text = body.decode("utf-8") if isinstance(body, bytes) else body
                    parsed_body = json.loads(text)
                except (json.JSONDecodeError, ValueError):
                    pass
            group_id = parsed_body.get("group_id", "mock-group")
            task_id = parsed_body.get("task_id", "mock-task")
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
