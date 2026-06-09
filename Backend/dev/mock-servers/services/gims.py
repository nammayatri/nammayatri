"""GIMS (Government Integrated Management System) fleet-operator + Nandi GTFS mock.

Handles all endpoints the BPP calls via the DIRECT IntegratedBPPConfig baseUrl
(http://localhost:8080) for the FRFSFleetOperatorFlow integration test.

GIMS fleet-operator endpoints (conductor trip management):
  POST /internal/fleet-operator/{gtfs_id}/currentOperation
  POST /internal/fleet-operator/{gtfs_id}/currentTripDetails
  POST /internal/fleet-operator/{gtfs_id}/tripAction
  POST /internal/fleet-operator/{gtfs_id}/employee/login
  POST /internal/fleet-operator/{gtfs_id}/verify

Nandi GTFS read endpoints (called by getV2FrfsRoute / getRouteByRouteId):
  GET  /route/{gtfs_id}/{route_id}
  GET  /route-stop-mapping/{gtfs_id}/route/{route_code}
  GET  /example-trip/{gtfs_id}/{route_id}
"""

import json


def handle(handler, path, body):
    # Parse request body (may be empty for GET calls)
    req = {}
    if body:
        try:
            text = body.decode("utf-8") if isinstance(body, bytes) else body
            req = json.loads(text)
        except (json.JSONDecodeError, ValueError):
            pass

    # ── GIMS fleet-operator (POST) ────────────────────────────────────────────
    if path.endswith("/currentOperation"):
        _current_operation(handler, req)
    elif path.endswith("/currentTripDetails"):
        _current_trip_details(handler, req)
    elif path.endswith("/tripAction"):
        _trip_action(handler, req)
    elif path.endswith("/login"):
        _employee_login(handler, req)
    elif path.endswith("/verify"):
        _verify(handler, req)
    # ── Nandi GTFS read (GET) ─────────────────────────────────────────────────
    elif "/route-stop-mapping/" in path and "/route/" in path:
        # GET /route-stop-mapping/{gtfs_id}/route/{route_code}
        _route_stop_mapping(handler, path)
    elif "/example-trip/" in path:
        # GET /example-trip/{gtfs_id}/{route_id}
        _example_trip(handler, path)
    elif "/route/" in path and "/route-stop-mapping/" not in path:
        # GET /route/{gtfs_id}/{route_id}
        _route_by_id(handler, path)
    else:
        handler._json({"status": "ok", "mock": True, "path": path})


# ── GIMS fleet-operator handlers ──────────────────────────────────────────────

def _current_operation(handler, req):
    # GimsCurrentOperationResp: {waybill_no, number_of_trips}
    handler._json({
        "waybill_no": "WAYBILL001",
        "number_of_trips": 5
    })


def _current_trip_details(handler, req):
    # GimsCurrentTripDetailsResp (snake_case as expected by Haskell FromJSON)
    handler._json({
        "waybill_no": "WAYBILL001",
        "vehicle_number": "TN01AB1234",
        "conductor_token": req.get("conductor_token"),
        "driver_token": req.get("driver_token"),
        "history": [],
        "current": {
            "trip_number": 1,
            "route_id": "10",
            "route_number": "10",
            "route_name": "Chennai Bus Route",
            "is_active_trip": True,
            "duty_date": "2026-06-09",
            "start_time": "09:00",
            "end_time": None
        },
        "upcoming": [
            {
                "trip_number": 2,
                "route_id": "10",
                "route_number": "10",
                "route_name": "Chennai Bus Route",
                "is_active_trip": False,
                "duty_date": "2026-06-09",
                "start_time": "11:00",
                "end_time": None
            }
        ]
    })


def _trip_action(handler, req):
    # GimsTripActionResp: Value (any JSON)
    handler._json({"status": "ok"})


def _employee_login(handler, req):
    # GimsEmployeeLoginResp: {verified, token, role}
    # role="conductor" so the backend creates the person as BUS_CONDUCTOR not BUS_DRIVER
    handler._json({
        "verified": True,
        "token": "gims-mock-token-001",
        "role": "conductor"
    })


def _verify(handler, req):
    # GimsVerifyResp
    handler._json({"verified": True})


# ── Nandi GTFS read handlers ──────────────────────────────────────────────────
# Stop codes must be consistent across all three responses so the route
# handler can join them by stopCode when building FRFSStationAPI entries.

_STOPS = [
    {"code": "S001", "name": "Central Bus Stand",  "lat": 13.0827, "lon": 80.2707, "seq": 1, "arr": 32400, "dep": 32400},
    {"code": "S002", "name": "Egmore Stop",         "lat": 13.0800, "lon": 80.2750, "seq": 2, "arr": 32700, "dep": 32700},
    {"code": "S003", "name": "Poonamallee Stop",    "lat": 13.1200, "lon": 80.3000, "seq": 3, "arr": 33300, "dep": 33300},
]


def _route_by_id(handler, path):
    # RouteInfoNandi: id, shortName, longName, mode, startPoint, endPoint, etc.
    # route.id becomes FRFSRouteAPI.code, so use the route code from the path
    parts = [p for p in path.split("/") if p]
    route_id = parts[-1] if parts else "10"
    handler._json({
        "id": route_id,
        "shortName": route_id,
        "longName": "Chennai Bus Route " + route_id,
        "mode": "BUS",
        "agencyName": "STCL",
        "tripCount": 5,
        "startPoint": {"lat": _STOPS[0]["lat"], "lon": _STOPS[0]["lon"]},
        "endPoint":   {"lat": _STOPS[-1]["lat"], "lon": _STOPS[-1]["lon"]},
        "stopCount": len(_STOPS),
        "serviceTierType": None
    })


def _route_stop_mapping(handler, path):
    # [RouteStopMappingInMemoryServer] — one entry per stop on the route
    result = []
    for s in _STOPS:
        result.append({
            "estimatedTravelTimeFromPreviousStop": None if s["seq"] == 1 else (s["arr"] - _STOPS[s["seq"] - 2]["arr"]),
            "providerCode": s["code"],
            "routeCode": "10",
            "sequenceNum": s["seq"],
            "stopCode": s["code"],
            "stopName": s["name"],
            "stopPoint": {"lat": s["lat"], "lon": s["lon"]},
            "vehicleType": "BUS",
            "hindiName": None,
            "regionalName": None,
            "parentStopCode": None,
            "gates": None
        })
    handler._json(result)


def _example_trip(handler, path):
    # TripDetails: {tripId, stops: [TripStopDetail]}
    # TripStopDetail uses custom FromJSON: stopId, stopCode, stopName, lat, lon,
    # scheduledArrival, scheduledDeparture, stopPosition (field name "sequence" in JSON)
    stops = []
    for s in _STOPS:
        stops.append({
            "stopId": s["code"],
            "stopCode": s["code"],
            "stopName": s["name"],
            "lat": s["lat"],
            "lon": s["lon"],
            "scheduledArrival": s["arr"],
            "scheduledDeparture": s["dep"],
            "stopPosition": s["seq"]
        })
    handler._json({
        "tripId": "TRIP-10-001",
        "stops": stops
    })
