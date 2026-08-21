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
import os
import uuid
import subprocess
from datetime import datetime, timedelta
from urllib.parse import parse_qs, urlparse
from status_store import extract_path_ids, deep_merge, register_body_decoder

# Deliberately NOT the key in the seeded integrated_bpp_config, and that is the whole
# point of this value. CDAC issues the AES key at /connect/token and versions it with
# key_index; rider-app used to ignore it and encrypt with a static config key instead.
# While the two strings matched, every local test passed identically whether the app read
# the issued key or the config one — the test proved nothing.
#
# This key is issued at auth AND used for every encrypt/decrypt below, so it is now the
# only key that works. A green run therefore means the app used what CDAC handed it. If
# someone reverts to the config key, the local suite fails with a decryption error, which
# is exactly the failure production would see after a CMRL key rotation.
_MOCK_AES_KEY = "IssuedAtAuthNotFromConfig_012345"


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
        # V2 sends encrypted payload in a field. "request" is the one rider-app actually
        # uses (EncryptedReq in CMRL/V2/GetFare.hs); without it nothing v2 sends was ever
        # decoded here, so body-matched overrides silently matched nothing and handlers
        # saw only the opaque envelope.
        for field in ("request", "data", "encryptedData", "payload"):
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

    # V2 stations: GET /api/qr/v1/stations/list?operatorNameId=
    #
    # Real records, copied from a live CMRL response captured 2026-08-13 (the full 54
    # are at hurl/frfs-seller/reference/cmrl_v2_stations_list.json). They are verbatim
    # on purpose: the FRFS SELLER mints item.id/fulfillment.id by stripping stationName
    # to alphanumerics, so a plausible-looking placeholder here would mint a plausible-
    # looking WRONG id and the local test would pass while production broke. That is
    # exactly what happened when names were read from GTFS instead of CMRL.
    #
    # Note latitude/longitude are strings and stationUniqueid is the bare numeric half —
    # the stop code callers use is stationShortName + "|" + stationUniqueid.
    if "stations" in path_lower and "list" in path_lower:
        base = {
            "stations": [
                {"stationName": "Washermanpet Metro", "stationShortName": "SWA", "stationUniqueid": "0101", "lineId": 1, "lineName": "Corridor_I", "latitude": "13.107064", "longitude": "80.280528", "sequenceNo": 1, "stationNameTamil": "வண்ணாரப்பேட்டை மெட்ரோ"},
                {"stationName": "Thousand Lights Metro", "stationShortName": "STL", "stationUniqueid": "0113", "lineId": 1, "lineName": "Corridor_I", "latitude": "13.058198", "longitude": "80.258056", "sequenceNo": 6, "stationNameTamil": "ஆயிரம் விளக்கு மெட்ரோ"},
                {"stationName": "Chennai International Airport Metro", "stationShortName": "SAP", "stationUniqueid": "0133", "lineId": 1, "lineName": "Corridor_I", "latitude": "12.980826", "longitude": "80.1642", "sequenceNo": 15, "stationNameTamil": "சென்னை சர்வதேச விமான நிலையம் மெட்ரோ"},
                {"stationName": "Wimco Nagar Depot Metro", "stationShortName": "SWD", "stationUniqueid": "0135", "lineId": 1, "lineName": "Corridor_I", "latitude": "13.1842985", "longitude": "80.30909273", "sequenceNo": 16, "stationNameTamil": "விம்கோ நகர் டிப்போ மெட்ரோ"},
                {"stationName": "Puratchi Thalaivi Dr. J. Jayalalithaa CMBT Metro", "stationShortName": "SCM", "stationUniqueid": "0221", "lineId": 2, "lineName": "Corridor_II", "latitude": "13.068568", "longitude": "80.203882", "sequenceNo": 35, "stationNameTamil": "புரட்சித் தலைவி  டாக்டர்  ஜெ.ஜெயலலிதா புறநகர் பேருந்து நிலையம் மெட்ரோ"},
                # Nulls are normal here — 3 of the real 54 look exactly like this, and
                # the response is parsed all-or-nothing, so one of them must be present
                # to keep the optional fields honest. Verbatim from the capture.
                {"stationName": "Chrompet Metro", "stationShortName": "CHM", "stationUniqueid": "702", "lineId": 7, "lineName": "Corridor 7", "latitude": None, "longitude": None, "sequenceNo": 68, "stationNameTamil": None},
                # Not a real record: lineId/sequenceNo omitted entirely, to prove a
                # station missing fields the item id does not depend on cannot take the
                # whole catalog down with it.
                {"stationName": "Sparse Field Metro", "stationShortName": "SPF", "stationUniqueid": "0999", "lineName": None, "latitude": None, "longitude": None, "stationNameTamil": None},
            ]
        }
        # The seven above are CURATED: each one carries an edge case (null coordinates,
        # absent lineId/sequenceNo) that the parser must survive, and a real roster has
        # none of them adjacent. They are the right default.
        #
        # But a seven-station roster cannot be compared against what the live service
        # publishes, and the discovery catalog IS the whole roster grouped by line. Drop a
        # captured station-list response at CMRL_STATION_ROSTER (default
        # /tmp/cmrl_station_roster.json) to serve that instead, which is the only way to
        # diff our on_search against the real one stop for stop. A path, not a flag, so
        # the roster under test is always the one you can go and read.
        roster_path = os.environ.get("CMRL_STATION_ROSTER", "/tmp/cmrl_station_roster.json")
        if os.path.exists(roster_path):
            with open(roster_path) as fh:
                captured = json.load(fh)
            stations = captured.get("stations") if isinstance(captured, dict) else captured
            base = {"stations": stations}
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)
        return

    # V2 business hours: GET /api/qr/v1/operations/business-date-hour?operatorNameId=
    #
    # paramName spellings are CMRL's, NOT the seller's. The two booking-restriction params
    # use SPACES where the published tag codes use UNDERSCORES, and the other five are
    # identical on both sides. Tidying these into underscores would make the mapping in
    # V2/BusinessHours.hs untestable: every name would match, and the only two that need
    # translating are exactly the two that would then look fine here and vanish in prod.
    #
    # Values are "dd-MM-yyyy HH:mm:ss" in IST, dated TODAY so what the seller publishes is
    # a live window rather than a fixture that expired. Selling and the booking
    # restriction end AFTER midnight, on purpose: the published tags keep the date
    # precisely because these windows cross it, and a mock that ended everything before
    # 23:59 would never exercise that.
    if "business-date-hour" in path_lower:
        today = datetime.now()
        d0 = today.strftime("%d-%m-%Y")
        d1 = (today + timedelta(days=1)).strftime("%d-%m-%Y")
        base = {
            "returnCode": "0",
            "returnMessage": "Success",
            "commonParamList": [
                {"paramId": 1, "paramName": "BUSINESS_DATE", "paramValue": f"{d0} 00:00:00"},
                {"paramId": 2, "paramName": "OPERATION_HOURS_START_TIME", "paramValue": f"{d0} 05:00:00"},
                {"paramId": 3, "paramName": "OPERATION_HOURS_END_TIME", "paramValue": f"{d1} 00:30:00"},
                {"paramId": 4, "paramName": "TICKET_SELLING_START_TIME", "paramValue": f"{d0} 04:30:00"},
                {"paramId": 5, "paramName": "TICKET_SELLING_END_TIME", "paramValue": f"{d1} 02:00:00"},
                {"paramId": 6, "paramName": "TICKET BOOKING RESTRICTION START TIME", "paramValue": f"{d0} 23:00:00"},
                {"paramId": 7, "paramName": "TICKET BOOKING RESTRICTION END TIME", "paramValue": f"{d1} 02:00:00"},
            ],
        }
        if extra:
            base = deep_merge(base, extra)
        handler._json(base)
        return

    # V2 fare: POST /api/qr/v1/fare/getfare
    #
    # Priced per ticketTypeId, because the seller publishes one item per journey type
    # and a flat fare would make SJT and RJT indistinguishable locally — the catalog
    # would look right while the return ticket was silently priced as a single.
    # Values measured against CMRL's own fare API on 2026-08-13 for 0101 -> 0133:
    #   ticketTypeId 1 (Go's SJT) and 105 (rider-app's config) -> 50 less 10 = 40
    #   ticketTypeId 102 (RJT)                                 -> 100 less 20 = 80
    if "getfare" in path_lower:
        req = _decode_cmrl_body(body) if body else {}
        ticket_type_id = req.get("ticketTypeId", 1) if isinstance(req, dict) else 1
        before, discount = (100.0, 20.0) if ticket_type_id == 102 else (50.0, 10.0)
        final = before - discount
        fare_data = json.dumps([{
            "fromStationId": "0001", "toStationId": "0002",
            "fareBeforeDiscount": before, "discountAmount": discount, "fareAfterDiscount": final,
            "cgst": 0.0, "sgst": 0.0, "finalFare": final,
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
    #
    # ONE TICKET PER grp_Size, because the number of QRs is what the caller counts.
    # CMRL issues a distinct QR per passenger, and the FRFS seller turns each into its
    # own Beckn fulfillment with its own authorization token — so a mock that always
    # answered with a single ticket would make a 6-ticket order publish 1 fulfillment
    # and still look like success. It also prices on the tickets ISSUED, so a fixed
    # count would silently under-bill.
    #
    # MERCHANT_ORDER_ID is echoed rather than invented: it is the caller's own order id,
    # and confirm asserts on getting it back.
    if ("createorder" in path_lower or "generate" in path_lower) and "v1" in path_lower:
        req = _decode_cmrl_body(body) if body else {}
        # ticketBlock.dynamic_Block.operators[].ticketInfo[] — CMRL's own nesting, and the
        # field names are the Haskell record names verbatim (no aeson prefix stripping).
        ticket_info = {}
        try:
            operators = req["ticketBlock"]["dynamic_Block"]["operators"]
            ticket_info = operators[0]["ticketInfo"][0]
        except (KeyError, IndexError, TypeError):
            pass
        try:
            quantity = max(1, int(ticket_info.get("grp_Size", 1)))
        except (TypeError, ValueError):
            quantity = 1
        merchant_order_id = (ticket_info.get("operatorData") or {}).get("merchantOrderId") or f"MO-{uuid.uuid4().hex[:8].upper()}"
        order_data = json.dumps({
            "returnCode": "0", "returnMessage": "Success",
            "Ticket_Response": [
                {
                    "QR_Payload": {"QR_Signature": f"mock-sig-{uuid.uuid4().hex[:16]}", "QR_SVC": "mock-svc", "QR_Tkt_Block": f"mock-tkt-{uuid.uuid4().hex[:12]}"},
                    "QR_Tkt_Sl_No": f"CMRL-{uuid.uuid4().hex[:8].upper()}", "QR_SHA256": f"mock-sha256-{uuid.uuid4().hex[:16]}",
                    "Merchant_Order_Id": merchant_order_id,
                    "Interchange_Status": "N", "Interchange_Stations": "", "Platform_No": "1",
                    "Ticket_Generation_Time": "2026-01-01T00:00:00", "Ticket_Validity_Time": "2027-01-01T00:00:00",
                }
                for _ in range(quantity)
            ],
        })
        handler._json({"response": _aes_encrypt(order_data)})
        return

    # V2 per-ticket details: GET /api/qr/v1/tickets/details-by-ticketId
    #                            ?operatorNameId=&ticketId=
    #
    # PLAIN JSON ARRAY, not the {"response": <aes>} envelope the fare and generate routes
    # use. Go reads it straight into []ChennaiV2TicketDetailsResponse with no decryption
    # (chennai_metro_v2.go:992), and this is the shape the seller's status path parses.
    #
    # statusCode is USED, deliberately NOT "NEW". "NEW" is what confirm already persisted,
    # so a mock answering "NEW" would make on_status identical to on_confirm and the test
    # would pass just as well with the whole refresh path deleted. USED maps through
    # chennaiV2TicketStatusMap to CLAIMED, so a green assertion proves the operator was
    # actually asked. Override to CANCELLED to exercise token blanking:
    #
    #   POST /mock/override {"service": "cmrl", "response": {"statusCode": "CANCELLED"}}
    #
    # returnCode is present and "0" because the caller gates on it — CDAC signals failure
    # with HTTP 200 and a non-zero code, so a response without it must not be trusted.
    if "details-by-ticketid" in path_lower:
        detail = {
            # Echoed so the mock log shows WHICH ticket was asked about — the seller must
            # send CMRL's QR_Tkt_Sl_No here, and sending the wrong id looks identical to a
            # healthy call that simply found nothing.
            #
            # handler.path, not path: server.py strips the query string before dispatch
            # (parsed.path), so reading it off `path` silently echoes the placeholder for
            # every request and the diagnostic lies.
            "ticketId": parse_qs(urlparse(handler.path).query).get("ticketId", ["MOCK-TICKET"])[0],
            "statusCode": "USED",
            "returnCode": "0",
            "returnMsg": "Success",
        }
        if extra:
            detail = deep_merge(detail, extra)
        handler._json([detail])
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
