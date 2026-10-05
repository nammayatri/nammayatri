"""PayTM payment mock.

Covers two surfaces:

1. Notification / generic PayTM calls  -> the original SUCCESS stub.
2. PayTM **EDC** (booth / special-zone Paytm-EDC flow) -> the `/ecr/*` endpoints
   the rider-app hits via `Kernel.External.Payment.PaytmEDC.Flow`:
     POST {baseUrl}/ecr/generateChecksum   (GenerateChecksumResp)
     POST {baseUrl}/ecr/payment/request    (sale     -> PaytmEDCResponse)
     POST {baseUrl}/ecr/payment/status     (status   -> PaytmEDCResponse)
     POST {baseUrl}/ecr/abort/txn          (abort    -> PaytmEDCResponse)

   For the EDC integration test the merchant_service_config `Payment_PaytmEDC`
   row must set `baseUrl` to `{mockServerUrl}/paytm` so these land here.

   `createOrder` only hard-requires `body.checksum` from generateChecksum
   (it `fromMaybeM`s on it); the sale/status responses just need to parse as a
   PaytmEDCResponse. Real settlement is driven by the S2S callback the test
   POSTs to /s2s/payment/paytm/edc/callback, not by these responses.

The `/mock/override` response dict is deep-merged into whatever this returns,
so a test can force a checksum/sale failure if it needs the negative path.
"""

from status_store import extract_path_ids, deep_merge


def _result_info(status="S"):
    # Mirrors PaytmEDCResultInfo (resultStatus/resultCode/resultMsg/resultCodeId).
    return {
        "resultStatus": status,
        "resultCode": "0000",
        "resultMsg": "Success",
        "resultCodeId": "0000",
    }


def _response_head():
    # Mirrors PaytmEDCResponseHead (all Maybe fields).
    return {"responseTimeStamp": "2026-01-01 00:00:00", "channelId": "EDC", "version": "1.0"}


def _checksum_response():
    # GenerateChecksumResp { head, body { checksum, resultInfo } }
    return {
        "head": _response_head(),
        "body": {"checksum": "MOCK_EDC_CHECKSUM", "resultInfo": _result_info("S")},
    }


def _edc_txn_response():
    # PaytmEDCResponse for sale / status / abort. Kept permissive; the fields the
    # rider-app reads are result status + the echoed ids. Extend if a stricter
    # decode is needed for a given assertion.
    return {
        "head": _response_head(),
        "body": {
            "resultInfo": _result_info("A"),  # "A" = Accepted at the terminal
            "merchantTransactionId": None,
            "transactionId": None,
        },
    }


def handle(handler, path, body):
    path_ids = extract_path_ids(path)
    override_status, extra = handler._get_override("paytm", *path_ids)

    p = path.lower()
    if "generatechecksum" in p:
        base = _checksum_response()
    elif "/ecr/payment/request" in p or "/ecr/payment/status" in p or "/ecr/abort" in p:
        base = _edc_txn_response()
    else:
        # Original generic PayTM stub (notifications etc.)
        base = {
            "status": override_status or "SUCCESS",
            "resultInfo": {"resultCode": "0000"},
        }

    if extra:
        base = deep_merge(base, extra)
    handler._json(base)
