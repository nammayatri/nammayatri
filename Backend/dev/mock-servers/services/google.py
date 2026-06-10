"""Mock Google Maps APIs — replaces the Haskell mock-google-exe (port 8019).

Routes (mounted under any path containing "/maps"):
  GET  /maps/distancematrix/json
  GET  /maps/directions/json
  GET  /maps/place/autocomplete/json
  GET  /maps/geocode/json
  GET  /maps/roads/snapToRoads

Returns minimal Google-shaped responses. The Haskell mock had a hand-curated
Place table to make distances deterministic per origin/destination; the Python
port returns a constant distance/duration. If a test needs specific values for
a city pair, install an explicit `POST /mock/override` rule on service "google".
"""

from urllib.parse import parse_qs, urlparse


def _qp(handler):
    return parse_qs(urlparse(handler.path).query)


def _distance_matrix(handler):
    qp = _qp(handler)
    origins = (qp.get("origins", [""])[0] or "").split("|")
    destinations = (qp.get("destinations", [""])[0] or "").split("|")

    def el():
        return {
            "distance": {"text": "5 km", "value": 5000},
            "duration": {"text": "10 mins", "value": 600},
            "status": "OK",
        }

    rows = [{"elements": [el() for _ in destinations]} for _ in origins]
    return {
        "destination_addresses": destinations,
        "origin_addresses": origins,
        "rows": rows,
        "status": "OK",
    }


def _directions(handler):
    qp = _qp(handler)
    origin = qp.get("origin", [""])[0]
    destination = qp.get("destination", [""])[0]

    def _ll(s):
        try:
            lat, lon = s.split(",")
            return {"lat": float(lat), "lng": float(lon)}
        except (ValueError, AttributeError):
            return {"lat": 12.97, "lng": 77.59}

    start_loc = _ll(origin)
    end_loc = _ll(destination)
    leg = {
        "distance": {"text": "5 km", "value": 5000},
        "duration": {"text": "10 mins", "value": 600},
        "end_address": destination,
        "end_location": end_loc,
        "start_address": origin,
        "start_location": start_loc,
        "steps": [
            {
                "distance": {"text": "5 km", "value": 5000},
                "duration": {"text": "10 mins", "value": 600},
                "end_location": end_loc,
                "html_instructions": "Head to destination",
                "polyline": {"points": "_p~iF~ps|U_ulLnnqC_mqNvxq`@"},
                "start_location": start_loc,
                "travel_mode": "DRIVING",
            }
        ],
    }
    route = {
        "bounds": {"northeast": end_loc, "southwest": start_loc},
        "copyrights": "Mock data",
        "legs": [leg],
        "overview_polyline": {"points": "_p~iF~ps|U_ulLnnqC_mqNvxq`@"},
        "summary": "Mock route",
        "warnings": [],
        "waypoint_order": [],
    }
    return {"geocoded_waypoints": [], "routes": [route], "status": "OK"}


def _autocomplete(handler):
    qp = _qp(handler)
    inp = qp.get("input", [""])[0]
    return {
        "predictions": [
            {
                "description": f"{inp} — Mock Place",
                "matched_substrings": [{"length": len(inp or "?"), "offset": 0}],
                "place_id": "mock_place_id",
                "reference": "mock_reference",
                "structured_formatting": {
                    "main_text": inp or "Mock Place",
                    "secondary_text": "Mock City, India",
                },
                "terms": [{"offset": 0, "value": inp or "Mock"}],
                "types": ["geocode"],
            }
        ],
        "status": "OK",
    }


def _geocode(handler):
    qp = _qp(handler)
    latlng = qp.get("latlng", ["12.97,77.59"])[0]
    try:
        lat, lon = (float(x) for x in latlng.split(","))
    except (ValueError, AttributeError):
        lat, lon = 12.97, 77.59
    return {
        "results": [
            {
                "address_components": [
                    {"long_name": "Mock Area", "short_name": "Mock", "types": ["sublocality"]},
                    {"long_name": "Mock City", "short_name": "Mock", "types": ["locality"]},
                    {"long_name": "Mock State", "short_name": "MS", "types": ["administrative_area_level_1"]},
                    {"long_name": "India", "short_name": "IN", "types": ["country"]},
                ],
                "formatted_address": "Mock Address, Mock City, Mock State, India",
                "geometry": {"location": {"lat": lat, "lng": lon}, "location_type": "APPROXIMATE"},
                "place_id": "mock_place_id",
                "types": ["street_address"],
            }
        ],
        "status": "OK",
    }


def _snap_to_road(handler):
    qp = _qp(handler)
    raw = qp.get("path", [""])[0]
    pts = []
    for chunk in raw.split("|"):
        try:
            lat, lon = chunk.split(",")
            pts.append({"latitude": float(lat), "longitude": float(lon)})
        except (ValueError, AttributeError):
            continue
    if not pts:
        pts = [{"latitude": 12.97, "longitude": 77.59}]
    return {
        "snappedPoints": [
            {"location": p, "originalIndex": i, "placeId": "mock_place_id"}
            for i, p in enumerate(pts)
        ],
        "warningMessage": "",
    }


def _compute_routes(handler):
    """Google Routes API v2 (POST /directions/v2:computeRoutes). Response shape differs
    entirely from the classic Directions API — LTS/driver-app consume distanceMeters,
    duration and polyline.encodedPolyline."""
    return {
        "routes": [
            {
                "distanceMeters": 5000,
                "duration": "600s",
                "staticDuration": "600s",
                "polyline": {"encodedPolyline": "_p~iF~ps|U_ulLnnqC_mqNvxq`@"},
            }
        ]
    }


def handle(handler, path, body):
    if handler.command == "GET" and (path.endswith("/maps") or path.endswith("/maps/") or path.endswith("/health")):
        return handler._json({"status": "UP", "service": "mock-google"})

    if "computeRoutes" in path or "/directions/v2" in path:
        return handler._json(_compute_routes(handler))
    if "/distancematrix" in path:
        return handler._json(_distance_matrix(handler))
    if "/directions" in path:
        return handler._json(_directions(handler))
    if "/place/autocomplete" in path:
        return handler._json(_autocomplete(handler))
    if "/geocode" in path:
        return handler._json(_geocode(handler))
    if "/snapToRoads" in path or "/roads" in path:
        return handler._json(_snap_to_road(handler))

    return handler._json({"status": "ZERO_RESULTS", "results": [], "mock": True, "path": path})
