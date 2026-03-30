"""CMRL (Chennai Metro Rail) mock — v1 and v2 auth, fare, QR tickets, status.

Overrides are applied automatically by the middleware via handler._request_override.
Collections use POST /mock/override to set response field overrides by request matching:

  POST /mock/override {
    "service": "cmrl",
    "extract": "body.customer_mobile",
    "value": "9876543210",
    "response": {"returnCode": "500", "returnMessage": "Technical Failure", "Ticket_Response": []}
  }
"""

import json
import uuid
import subprocess
from status_store import extract_path_ids, deep_merge, register_body_decoder

_MOCK_AES_KEY = "MockAES256Key123456789012345ABCD"


def _aes_decrypt(ciphertext_b64, key=_MOCK_AES_KEY):
    """AES-256-CBC decrypt base64 ciphertext via openssl."""
    key_hex = key.encode("utf-8").hex()
    iv_hex = "00" * 16
    result = subprocess.run(
        ["openssl", "enc", "-d", "-aes-256-cbc", "-base64", "-A", "-K", key_hex, "-iv", iv_hex],
        input=ciphertext_b64.encode("utf-8"), capture_output=True,
    )
    if result.returncode != 0:
        return None
    return result.stdout.decode("utf-8")


def _decode_cmrl_body(body_raw):
    """Decode CMRL encrypted request: try JSON first, then decrypt if it has encrypted field."""
    text = body_raw.decode("utf-8") if isinstance(body_raw, bytes) else body_raw
    try:
        req = json.loads(text)
        # V2 sends encrypted payload in a field
        for field in ("data", "encryptedData", "payload"):
            if field in req and isinstance(req[field], str):
                decrypted = _aes_decrypt(req[field])
                if decrypted:
                    try:
                        return json.loads(decrypted)
                    except json.JSONDecodeError:
                        pass
        return req  # V1 sends plain JSON
    except json.JSONDecodeError:
        return {}


register_body_decoder("cmrl", _decode_cmrl_body)


def _aes_encrypt(plaintext, key=_MOCK_AES_KEY):
    key_hex = key.encode("utf-8").hex()
    iv_hex = "00" * 16
    result = subprocess.run(
        ["openssl", "enc", "-aes-256-cbc", "-base64", "-A", "-K", key_hex, "-iv", iv_hex],
        input=plaintext.encode("utf-8"), capture_output=True,
    )
    if result.returncode != 0:
        raise RuntimeError(f"openssl encrypt failed: {result.stderr.decode()}")
    return result.stdout.decode("utf-8").strip()


def handle(handler, path, body):
    path_ids = extract_path_ids(path)
    override_status, extra = handler._get_override("cmrl", *path_ids)
    path_lower = path.lower()

    # CMRL V2 auth: POST /api/qr/v1/connect/token
    if "connect" in path_lower and "token" in path_lower:
        base = {
            "access_token": f"mock-cmrlv2-token-{uuid.uuid4().hex[:8]}",
            "expires_in": 3600,
            "token_type": "Bearer",
            "refresh_token": f"mock-refresh-{uuid.uuid4().hex[:8]}",
            "key_index": 1,
            "key": _MOCK_AES_KEY,
            "algo": "AES-256-CBC",
        }
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)
        return

    # CMRL V1 auth: POST /CmrlThirdParty/authenticate
    if "authenticate" in path_lower:
        base = {
            "statusCode": 0,
            "message": "Success",
            "result": {"access_token": f"mock-cmrl-token-{uuid.uuid4().hex[:8]}"},
        }
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)
        return

    # V2 fare: POST /api/qr/v1/fare/getfare
    if "getfare" in path_lower:
        fare_data = json.dumps([{
            "fromStationId": "0001", "toStationId": "0002",
            "fareBeforeDiscount": 30.0, "discountAmount": 0.0, "fareAfterDiscount": 30.0,
            "cgst": 0.0, "sgst": 0.0, "finalFare": 30.0,
            "fareValidTime": "2027-01-01 00:00:00",
            "fareQuotIdforOneTicket": f"FQ-{uuid.uuid4().hex[:8].upper()}",
            "returnCode": "0", "returnMsg": "Success",
        }])
        base = {"response": _aes_encrypt(fare_data)}
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)
        return

    # V2 order: POST /api/qr/v1/tickets/generate
    if ("createorder" in path_lower or "generate" in path_lower) and "v1" in path_lower:
        ticket_no = f"CMRL-{uuid.uuid4().hex[:8].upper()}"
        order_data = json.dumps({
            "returnCode": "0", "returnMessage": "Success",
            "Ticket_Response": [{
                "QR_Payload": {"QR_Signature": f"mock-sig-{uuid.uuid4().hex[:16]}", "QR_SVC": "mock-svc", "QR_Tkt_Block": f"mock-tkt-{uuid.uuid4().hex[:12]}"},
                "QR_Tkt_Sl_No": ticket_no, "QR_SHA256": f"mock-sha256-{uuid.uuid4().hex[:16]}",
                "Merchant_Order_Id": f"MO-{uuid.uuid4().hex[:8].upper()}",
                "Interchange_Status": "N", "Interchange_Stations": "", "Platform_No": "1",
                "Ticket_Generation_Time": "2026-01-01T00:00:00", "Ticket_Validity_Time": "2027-01-01T00:00:00",
            }],
        })
        handler._json({"response": _aes_encrypt(order_data)})
        return

    # V2 ticket status
    if "ticketstatus" in path_lower and "v1" in path_lower:
        status_data = json.dumps({
            "returnCode": "0", "returnMessage": "Success",
            "Ticket_Response": [{
                "QR_Payload": {"QR_Signature": "mock-sig", "QR_SVC": "mock-svc", "QR_Tkt_Block": "mock-tkt-block"},
                "QR_Tkt_Sl_No": "MOCK-001", "QR_SHA256": "mock-sha256", "Merchant_Order_Id": "MOCK-ORDER",
                "Interchange_Status": "N", "Interchange_Stations": "", "Platform_No": "1",
            }],
        })
        base = {"response": _aes_encrypt(status_data)}
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)
        return

    # V1 fare
    if "fare" in path_lower:
        base = {"returnCode": 0, "returnMsg": "Success", "ticketFare": 30, "totalFare": 30, "noOfTickets": 1}
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)
        return

    # V1 QR
    if "generateqr" in path_lower:
        ticket_no = f"CMRL-{uuid.uuid4().hex[:8].upper()}"
        base = {"returnCode": 0, "returnMsg": "Success", "qrRawData": f"mock-qr-{ticket_no}", "ticketNo": ticket_no, "transactionRefNo": f"TXN-{uuid.uuid4().hex[:6].upper()}"}
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)
        return

    # V1 ticket status
    if "ticketstatus" in path_lower:
        base = {"statusCode": 0, "message": "Success", "result": {"bookingRefNo": f"BR-{uuid.uuid4().hex[:8].upper()}", "bookingTs": "2026-01-01 00:00:00", "bookingTypeCode": "SJT", "passengerCount": 1, "maxTapInCount": 1, "maxTapOutCount": 1, "tapInCount": 0, "tapOutCount": 0}}
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)
        return

    # Default
    base = {"returnCode": 0, "returnMsg": "Success"}
    if extra:
        base = deep_merge(base, extra)
    handler._json(base)
