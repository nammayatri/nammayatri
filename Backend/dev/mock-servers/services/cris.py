"""CRIS (Delhi Metro/Subway) mock — auth, route fare, book journey.

Overrides are applied automatically by the middleware via handler._request_override.
For CRIS, the request body is encrypted — the middleware matches on the plaintext body fields.
For encrypted requests, collections should match on a known field like header.token or path segment.

Collections use POST /mock/override:
  {"service": "cris", "extract": "header.authorization", "value": "Bearer mock-cris-token-xxx",
   "response": {"respCode": 500, "respMessage": "Technical Failure"}}
"""

import json
import subprocess
import uuid
from status_store import extract_path_ids, deep_merge, register_body_decoder

_MOCK_ENCRYPT_KEY = "CRISEncryptKey12345678901234ABCD"
_MOCK_DECRYPT_KEY = "CRISDecryptKey12345678901234ABCD"
_MOCK_AGENT_KEY = "CRISAgentKey1234567890"


def _decode_cris_body(body_raw):
    """Decode CRIS encrypted request body: {app, data_} → decrypt data_ → parse JSON."""
    text = body_raw.decode("utf-8") if isinstance(body_raw, bytes) else body_raw
    req = json.loads(text)
    encrypted = req.get("data_") or req.get("data")
    if encrypted:
        decrypted = _aes_ecb_decrypt(encrypted, _MOCK_ENCRYPT_KEY)
        if decrypted:
            return json.loads(decrypted)
    return req


# Register so the override middleware can extract fields from encrypted CRIS requests
register_body_decoder("cris", _decode_cris_body)


def _aes_ecb_encrypt(plaintext, key):
    key_hex = key.encode("utf-8").hex()
    result = subprocess.run(
        ["openssl", "enc", "-aes-256-ecb", "-base64", "-A", "-K", key_hex, "-nosalt"],
        input=plaintext.encode("utf-8"), capture_output=True,
    )
    if result.returncode != 0:
        raise RuntimeError(f"openssl encrypt failed: {result.stderr.decode()}")
    return result.stdout.decode("utf-8").strip()


def _aes_ecb_decrypt(ciphertext_b64, key):
    key_hex = key.encode("utf-8").hex()
    result = subprocess.run(
        ["openssl", "enc", "-d", "-aes-256-ecb", "-base64", "-A", "-K", key_hex, "-nosalt"],
        input=ciphertext_b64.encode("utf-8"), capture_output=True,
    )
    if result.returncode != 0:
        return None
    return result.stdout.decode("utf-8")


def handle(handler, path, body):
    path_ids = extract_path_ids(path)
    override_status, extra = handler._get_override("cris", *path_ids)
    path_lower = path.lower()

    # POST /token
    if path_lower.rstrip("/").endswith("/token"):
        base = {"access_token": f"mock-cris-token-{uuid.uuid4().hex[:8]}", "token_type": "Bearer", "expires_in": 3600}
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)
        return

    # POST /get_route_fare_details
    if "route_fare" in path_lower or "get_route_fare" in path_lower:
        fare_data = json.dumps({
            "routeFareDetailsList": [{
                "routeId": 1,
                "fareDtlsList": [{"adultFare": "30", "childFare": "15", "distance": 10, "via": " ", "ticketTypeCode": "J", "trainTypeCode": "O", "classCode": "II"}],
                "maximumValuesList": [],
                "allowedValuesList": [{"ticketTypeCode": "J", "ticketTypeName": "Journey", "trainTypeCode": "O", "trainTypeDescription": "Ordinary", "classCode": "II"}],
            }],
            "sdkData": None,
        })
        base = {"responseCode": "0", "responseData": _aes_ecb_encrypt(fare_data, _MOCK_DECRYPT_KEY)}
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)
        return

    # POST /bookJrny
    if "bookjrny" in path_lower or ("book" in path_lower and "token" not in path_lower):
        ticket_no = f"CRIS-{uuid.uuid4().hex[:8].upper()}"
        mob = "0000000000"
        if body:
            try:
                req = json.loads(body)
                encrypted_data = req.get("data_") or req.get("data")
                if encrypted_data:
                    decrypted = _aes_ecb_decrypt(encrypted_data, _MOCK_ENCRYPT_KEY)
                    if decrypted:
                        booking_req = json.loads(decrypted)
                        mob = booking_req.get("mob", mob)
            except (json.JSONDecodeError, ValueError, TypeError):
                pass

        mobile_prefix = mob[:5]
        mobile_suffix = mob[-5:]
        derived_agent_key = mobile_suffix + _MOCK_AGENT_KEY + mobile_prefix

        agent_ticket = json.dumps({
            "utsNumber": ticket_no, "cashReceived": 30, "source": "SRC", "destination": "DST",
            "via": " ", "adult": 1, "child": 0, "classCode": "II", "ticketType": "J", "trainType": "O",
            "serviceTax": "0", "txnTime": "2026-01-01T00:00:00", "jrnyCommencingString": "Valid for travel",
            "showTicketValidity": "01/01/2027 23:59:00", "journeyDate": "2026-01-01",
            "routeMessage": None, "chargeableAmount": 30, "jrnyCommencingHour": 24,
        })
        booking_data = json.dumps({"ticketNo": ticket_no, "qrData": f"mock-qr-{ticket_no}"})

        base = {
            "respCode": 0, "respMessage": override_status or "SUCCESS",
            "encrypted": _aes_ecb_encrypt(booking_data, _MOCK_DECRYPT_KEY),
            "agentTicketData": _aes_ecb_encrypt(agent_ticket, derived_agent_key),
        }
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)
        return

    # Default
    base = {"responseCode": "0", "responseData": ""}
    if extra:
        base = deep_merge(base, extra)
    handler._json(base)
