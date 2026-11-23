"""OSRM road routing for the planner (car profile: real streets, one-way rules respected).

OSRM_URL (default http://localhost:5001, the local mobility stack's osrm-server). Every call returns None when OSRM is not
reachable or has no answer, so the planner falls back to straight-line estimates.
"""

import json
import os
import urllib.request

OSRM_URL = os.environ.get("OSRM_URL", "http://localhost:5001").rstrip("/")
TIMEOUT_S = 10


def _get(path):
    try:
        with urllib.request.urlopen(f"{OSRM_URL}{path}", timeout=TIMEOUT_S) as resp:
            body = json.load(resp)
        return body if body.get("code") == "Ok" else None
    except (OSError, ValueError):
        return None


def _coord(p):
    return f"{p['lon']:.6f},{p['lat']:.6f}"


def snap(point):
    """The nearest drivable road: {lat, lon, street, meters} (meters = how far the point moved), or None."""
    body = _get(f"/nearest/v1/driving/{_coord(point)}?number=1")
    if not body or not body.get("waypoints"):
        return None
    wp = body["waypoints"][0]
    return {"lon": wp["location"][0], "lat": wp["location"][1], "street": wp.get("name") or "", "meters": wp.get("distance", 0.0)}


def route(points):
    """The driving route through the points in order: {geometry: [[lat, lon]...], legSeconds, legMeters}, or None."""
    if len(points) < 2:
        return None
    body = _get(f"/route/v1/driving/{';'.join(_coord(p) for p in points)}?overview=full&geometries=geojson&steps=false&continue_straight=false")
    if not body or not body.get("routes"):
        return None
    r = body["routes"][0]
    return {
        "geometry": [[lat, lon] for lon, lat in r["geometry"]["coordinates"]],
        "legSeconds": [leg["duration"] for leg in r["legs"]],
        "legMeters": [leg["distance"] for leg in r["legs"]],
    }
