"""SAP Journal API mock.

Handles two endpoints routed under /sap prefix:
  - /sap/oauth2/api/v1/token  → OAuth token (JSON)
  - /sap/http/aws/s4/odata/ondc → Journal entry posting (XML in, XML out)
"""

import xml.etree.ElementTree as ET

from status_store import extract_path_ids, deep_merge


def handle(handler, path, body):
    path_ids = extract_path_ids(path)
    override_status, extra = handler._get_override("sap", *path_ids)

    if "/oauth2/" in path:
        _handle_token(handler, override_status, extra)
    elif "/odata/" in path:
        _handle_journal(handler, body, override_status, extra)
    else:
        handler._json({"error": "unknown SAP endpoint", "path": path}, status=404)


def _handle_token(handler, override_status, extra):
    base = {
        "access_token": "mock-sap-token-abc123",
        "token_type": "bearer",
        "expires_in": 3600,
        "scope": None,
        "jti": None,
    }
    if extra:
        base = deep_merge(base, extra)
    handler._json(base)


def _handle_journal(handler, body, override_status, extra):
    batch_ids = _extract_batch_ids(body)
    headers_xml = []
    for bid in batch_ids:
        headers_xml.append(
            "<Header>"
            "<Msgtyp>S</Msgtyp>"
            f"<BatchId>{bid}</BatchId>"
            "<Belnr>1000000001</Belnr>"
            "<Gjahr>2026</Gjahr>"
            "<Message>Document posted successfully</Message>"
            "</Header>"
        )
    if not headers_xml:
        headers_xml.append(
            "<Header>"
            "<Msgtyp>S</Msgtyp>"
            "<BatchId>0</BatchId>"
            "<Belnr>1000000001</Belnr>"
            "<Gjahr>2026</Gjahr>"
            "<Message>Document posted successfully</Message>"
            "</Header>"
        )
    xml_resp = (
        '<?xml version="1.0" encoding="UTF-8"?>'
        "<HeaderSet>" + "".join(headers_xml) + "</HeaderSet>"
    )
    handler._raw(200, "application/xml", xml_resp)


def _extract_batch_ids(body):
    if not body:
        return []
    try:
        text = body.decode("utf-8") if isinstance(body, bytes) else body
        root = ET.fromstring(text)
        ids = []
        for header in root.iter("Header"):
            bid_el = header.find("BatchId")
            if bid_el is not None and bid_el.text:
                ids.append(bid_el.text)
        return ids
    except Exception:
        return []
