"""EBIX (Kolkata Bus) mock — auth, save ticket, create QR.

Response types from code:
  Auth:          {access_token: Text, expires_in: Seconds}  (snake_case JSON)
  SaveMobTicket: {DATA: {MB_TKT_ID: Text}}  (custom JSON field mapping)
  CreateQr:      {qrData: Text} or encrypted string
"""

import uuid
from status_store import extract_path_ids, deep_merge


def handle(handler, path, body):
    path_ids = extract_path_ids(path)
    override_status, extra = handler._get_override("ebix", *path_ids)
    path_lower = path.lower()

    # GET /token → AuthRes {access_token, expires_in}
    if path_lower.rstrip("/").endswith("/token"):
        base = {
            "access_token": f"mock-ebix-token-{uuid.uuid4().hex[:8]}",
            "expires_in": 3600,
        }
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)
        return

    # GET /Api/V2/Cons/SaveMobTicket → SaveMobTicketRes {DATA: {MB_TKT_ID: Text}}
    if "savemobticket" in path_lower:
        ticket_id = f"EBIX-{uuid.uuid4().hex[:8].upper()}"
        base = {
            "DATA": {"MB_TKT_ID": ticket_id},
        }
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)
        return

    # GET /Api/V2/Cons/CreateQr → encrypted QR string
    if "createqr" in path_lower:
        base = {
            "qrData": f"mock-ebix-qr-{uuid.uuid4().hex[:12]}",
            "status": override_status or "success",
        }
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)
        return

    # Default
    base = {"status": override_status or "success"}
    if extra:
        base = deep_merge(base, extra)
    handler._json(base)
