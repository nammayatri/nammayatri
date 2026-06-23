"""Mock Signzy — challan search endpoint.

Routes (mounted under any path containing "/signzy"):
  POST  /signzy/api/v3/vehicle/challan-search
        — returns a fixed ChallanSearchResp with 3 "Pending" challans
          (plus 1 "Disposed", which the interface filters out → count = 3).

Response shape mirrors Kernel.External.ChallanSearch.Signzy.Types:
  ChallanSearchResp { result :: ChallanSearchResult }
  ChallanSearchResult { regNo, message, status_code, challanDetails,
                        successfulSources, failedSources }
  ChallanDetail { number, challanNumber, rto, accusedName, amount, challanStatus }
"""

import json

_CHALLAN_DETAILS = [
    {"number": 1, "challanNumber": "MOCK-CH-001", "rto": "KA01", "accusedName": "Mock Driver", "amount": "500", "challanStatus": "Pending"},
    {"number": 2, "challanNumber": "MOCK-CH-002", "rto": "KA01", "accusedName": "Mock Driver", "amount": "1000", "challanStatus": "Pending"},
    {"number": 3, "challanNumber": "MOCK-CH-003", "rto": "KA01", "accusedName": "Mock Driver", "amount": "200", "challanStatus": "Pending"},
    {"number": 4, "challanNumber": "MOCK-CH-004", "rto": "KA01", "accusedName": "Mock Driver", "amount": "750", "challanStatus": "Disposed"},
]


def handle(handler, path, body):
    text = body.decode("utf-8") if isinstance(body, bytes) and body else (body or "")
    try:
        body_json = json.loads(text) if text else {}
    except (json.JSONDecodeError, ValueError):
        body_json = {}

    if "/challan-search" in path:
        vehicle_number = body_json.get("vehicleNumber", "UNKNOWN") if isinstance(body_json, dict) else "UNKNOWN"
        return handler._json(
            {
                "result": {
                    "regNo": vehicle_number,
                    "message": "success",
                    "status_code": 200,
                    "challanDetails": _CHALLAN_DETAILS,
                    "successfulSources": ["mock"],
                    "failedSources": [],
                }
            }
        )

    return handler._json({"error": "Not found", "path": path}, status=404)
