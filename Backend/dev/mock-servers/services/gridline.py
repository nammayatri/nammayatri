"""Gridline (Aadhaar/bank account verification) mock."""

import uuid
from datetime import datetime, timezone
from urllib.parse import urlparse, parse_qs
from status_store import extract_path_ids, deep_merge


def handle(handler, path, body):
    path_ids = extract_path_ids(path)
    override_status, extra = handler._get_override("gridline", *path_ids)

    # Idfy async verify endpoints (POST .../tasks/async/...) return IdfySuccess
    if "tasks/async" in path:
        base = {
            "request_id": str(uuid.uuid4()),
        }
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)
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
