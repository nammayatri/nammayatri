"""Mock Google Maps APIs — replaces the Haskell mock-google-exe (port 8019).

Routes (mounted under any path containing "/maps"):
  GET  /maps/distancematrix/json
  GET  /maps/directions/json
  GET  /maps/place/autocomplete/json
  GET  /maps/place/details/json
  POST /maps/places/:autocomplete      (Places API New)
  GET  /maps/places/{placeId}          (Places API New)
  POST /maps/directions/v2:computeRoutes
  GET  /maps/geocode/json
  GET  /maps/roads/snapToRoads

Returns minimal Google-shaped responses. The Haskell mock had a hand-curated
Place table to make distances deterministic per origin/destination; the Python
port derives directions distance/duration from the straight-line distance between
origin and destination (x1.3 road factor, ~30 km/h), so fares follow the destination.
Place search returns real places near the rider from Photon (OpenStreetMap), falling
back to a few fixed Bangalore places when Photon can't be reached.
"""

import base64
import math
import os
import urllib.request
import json as _json
from urllib.parse import parse_qs, urlencode, urlparse


def _osrm_base_url():
    return os.environ.get("OSRM_HOST", "http://localhost:5001")


def _osrm_table(coords):
    # coords = list of "lat,lon" strings
    # OSRM expects lon,lat order
    coord_str = ";".join(
        f"{lon},{lat}" for lat, lon in (c.split(",") for c in coords)
    )
    url = f"{_osrm_base_url()}/table/v1/driving/{coord_str}?annotations=duration,distance"
    try:
        with urllib.request.urlopen(url, timeout=2) as resp:
            data = _json.loads(resp.read())
        return data.get("distances"), data.get("durations")
    except Exception:
        return None, None


def _qp(handler):
    return parse_qs(urlparse(handler.path).query)


def _distance_matrix(handler):
    qp = _qp(handler)
    origins = (qp.get("origins", [""])[0] or "").split("|")
    destinations = (qp.get("destinations", [""])[0] or "").split("|")

    # Try OSRM /table for real road distances.
    # Feed origins first then destinations; slice the origin rows and
    # destination columns out of the resulting N×N matrix.
    # Falls back to hardcoded 5000m if OSRM is unreachable.
    all_coords = origins + destinations
    distances, durations = _osrm_table(all_coords)

    def el(dist_val, dur_val):
        d = round(dist_val) if dist_val is not None else 5000
        t = round(dur_val) if dur_val is not None else 600
        return {
            "distance": {"text": f"{d} m", "value": d},
            "duration": {"text": f"{t // 60} mins", "value": t},
            "status": "OK",
        }

    def fallback_el():
        return {
            "distance": {"text": "5 km", "value": 5000},
            "duration": {"text": "10 mins", "value": 600},
            "status": "OK",
        }

    n_orig = len(origins)
    rows = []
    for i in range(n_orig):
        elements = []
        for j in range(len(destinations)):
            dest_col = n_orig + j
            try:
                dist_val = distances[i][dest_col] if distances else None
                dur_val = durations[i][dest_col] if durations else None
                elements.append(el(dist_val, dur_val))
            except (TypeError, IndexError):
                elements.append(fallback_el())
        rows.append({"elements": elements})

    return {
        "destination_addresses": destinations,
        "origin_addresses": origins,
        "rows": rows,
        "status": "OK",
    }


_ROAD_FACTOR = 1.3  # straight line -> rough road distance
_SPEED_MPS = 30 * 1000 / 3600


def _road_estimate(origin, destination):
    """(meters, seconds) from the straight-line distance between two "lat,lon" strings,
    so fares follow the destination; (5000, 600) when either point doesn't parse."""
    try:
        (lat1, lon1), (lat2, lon2) = ((float(x) for x in s.split(",")) for s in (origin, destination))
    except (ValueError, AttributeError):
        return 5000, 600
    meters = round(_straight_meters(lat1, lon1, lat2, lon2) * _ROAD_FACTOR)
    return meters, round(meters / _SPEED_MPS)


def _encode_polyline(points):
    """Google's encoded-polyline format for a list of (lat, lon)."""
    out, prev_lat, prev_lon = [], 0, 0
    for lat, lon in points:
        ilat, ilon = round(lat * 1e5), round(lon * 1e5)
        for delta in (ilat - prev_lat, ilon - prev_lon):
            v = ~(delta << 1) if delta < 0 else delta << 1
            while v >= 0x20:
                out.append(chr((0x20 | (v & 0x1F)) + 63))
                v >>= 5
            out.append(chr(v + 63))
        prev_lat, prev_lon = ilat, ilon
    return "".join(out)


# Points on the mock's straight route line. The driver app redraws the pickup
# route whenever the driver is more than 20 m off it, so the line needs points
# along the way, not just the two ends.
_ROUTE_POINTS = 20


def _straight_polyline(lat1, lon1, lat2, lon2):
    """A straight route from origin to destination, as an encoded polyline."""
    return _encode_polyline(
        [
            (lat1 + (lat2 - lat1) * i / _ROUTE_POINTS, lon1 + (lon2 - lon1) * i / _ROUTE_POINTS)
            for i in range(_ROUTE_POINTS + 1)
        ]
    )


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
    meters, seconds = _road_estimate(origin, destination)
    distance = {"text": f"{meters / 1000:.1f} km", "value": meters}
    duration = {"text": f"{seconds // 60} mins", "value": seconds}
    line = _straight_polyline(start_loc["lat"], start_loc["lng"], end_loc["lat"], end_loc["lng"])
    leg = {
        "distance": distance,
        "duration": duration,
        "end_address": destination,
        "end_location": end_loc,
        "start_address": origin,
        "start_location": start_loc,
        "steps": [
            {
                "distance": distance,
                "duration": duration,
                "end_location": end_loc,
                "html_instructions": "Head to destination",
                "polyline": {"points": line},
                "start_location": start_loc,
                "travel_mode": "DRIVING",
            }
        ],
    }
    route = {
        "bounds": {"northeast": end_loc, "southwest": start_loc},
        "copyrights": "Mock data",
        "legs": [leg],
        "overview_polyline": {"points": line},
        "summary": "Mock route",
        "warnings": [],
        "waypoint_order": [],
    }
    return {"geocoded_waypoints": [], "routes": [route], "status": "OK"}


# Place search is answered by Photon (https://photon.komoot.io), a free search
# over OpenStreetMap data: real places near the rider, in any city. PHOTON_URL
# points it at another Photon server (e.g. a self-hosted one).
_PHOTON_URL = os.environ.get("PHOTON_URL", "https://photon.komoot.io")
_PHOTON_TIMEOUT_S = 3
_PHOTON_LIMIT = 5
_USER_AGENT = "nammayatri-devbox-mock/1.0"

# A Photon result's id carries its point and label ("osm_<lat>_<lon>_<base64url label>"),
# so place details answer from the id alone, with no second call and no stored state.
_PLACE_ID_PREFIX = "osm_"
_MAX_LABEL_CHARS = 100


def _straight_meters(lat1, lon1, lat2, lon2):
    """Great-circle distance in meters."""
    p1, p2 = math.radians(lat1), math.radians(lat2)
    a = math.sin((p2 - p1) / 2) ** 2 + math.cos(p1) * math.cos(p2) * math.sin(math.radians(lon2 - lon1) / 2) ** 2
    return 2 * 6371000 * math.asin(math.sqrt(a))


def _photon_label(props):
    """"Name, street, area, city, state" from a Photon feature, without repeats or blanks."""
    street = " ".join(p for p in (props.get("housenumber"), props.get("street")) if p)
    parts = []
    for part in (props.get("name"), street, props.get("district"), props.get("city"), props.get("state")):
        if part and part not in parts:
            parts.append(part)
    return ", ".join(parts)[:_MAX_LABEL_CHARS]


def _photon_search(text, centre):
    """[(label, lat, lon)] from Photon for the typed text, nearest to centre first;
    [] when the text is blank or Photon fails, is slow or finds nothing."""
    if not text.strip():
        return []
    params = {"q": text, "limit": _PHOTON_LIMIT, "lang": "en"}
    if centre:
        params["lat"], params["lon"] = centre
    req = urllib.request.Request(f"{_PHOTON_URL}/api/?{urlencode(params)}", headers={"User-Agent": _USER_AGENT})
    try:
        with urllib.request.urlopen(req, timeout=_PHOTON_TIMEOUT_S) as resp:
            features = _json.loads(resp.read()).get("features", [])
        places = []
        for f in features:
            lon, lat = f["geometry"]["coordinates"][:2]
            label = _photon_label(f.get("properties", {}))
            if label and all(label != seen for seen, _, _ in places):
                places.append((label, float(lat), float(lon)))
        return places
    except Exception:
        return []


def _photon_place_id(label, lat, lon):
    encoded = base64.urlsafe_b64encode(label.encode()).decode().rstrip("=")
    return f"{_PLACE_ID_PREFIX}{lat:.6f}_{lon:.6f}_{encoded}"


def _place_from_photon_id(place_id):
    """(label, lat, lon) from an id made by _photon_place_id, or None."""
    if not place_id.startswith(_PLACE_ID_PREFIX):
        return None
    try:
        lat, lon, encoded = place_id[len(_PLACE_ID_PREFIX):].split("_", 2)
        label = base64.urlsafe_b64decode(encoded + "=" * (-len(encoded) % 4)).decode()
        return label, float(lat), float(lon)
    except (ValueError, UnicodeDecodeError):
        return None


def _parse_lat_lon(value):
    """(lat, lon) from a "lat,lon" string, or None."""
    try:
        lat, lon = (float(x) for x in value.split(","))
        return lat, lon
    except (ValueError, AttributeError):
        return None


def _centre_v2(req):
    """The rider's point in a Places API (New) autocomplete request: origin, else the
    centre of locationBias or locationRestriction, else None."""
    candidates = [req.get("origin")]
    for key in ("locationBias", "locationRestriction"):
        area = req.get(key)
        candidates.append(area.get("circle", {}).get("center") if isinstance(area, dict) else None)
    for point in candidates:
        try:
            return float(point["latitude"]), float(point["longitude"])
        except (KeyError, TypeError, ValueError):
            continue
    return None


def _search(text, centre):
    """[(place_id, label, meters or None)] for the rider's search: Photon's places
    near centre, else the fixed Bangalore places below so the rider always has a pick."""
    found = _photon_search(text, centre)
    if found:
        return [
            (_photon_place_id(label, lat, lon), label, round(_straight_meters(*centre, lat, lon)) if centre else None)
            for label, lat, lon in found
        ]
    return [(pid, name, None) for pid, name, _, _ in _matching_places(text)]


def _place_for_id(place_id):
    """(place_id, label, lat, lon) for a place id from _search."""
    photon = _place_from_photon_id(place_id)
    if photon:
        return (place_id, *photon)
    return _place_by_id(place_id)


# Fallback places when Photon gives nothing, with real Bangalore coordinates, so a
# picked pickup or drop gives a sensible route and fare.
_PLACES = [
    ("mock-blr-koramangala", "Koramangala, Bengaluru, Karnataka", 12.9352, 77.6245),
    ("mock-blr-indiranagar", "Indiranagar, Bengaluru, Karnataka", 12.9784, 77.6408),
    ("mock-blr-mg-road", "MG Road, Bengaluru, Karnataka", 12.9757, 77.6055),
    ("mock-blr-majestic", "Majestic Bus Stand, Bengaluru, Karnataka", 12.9767, 77.5713),
    ("mock-blr-whitefield", "Whitefield, Bengaluru, Karnataka", 12.9698, 77.7500),
    ("mock-blr-electronic-city", "Electronic City, Bengaluru, Karnataka", 12.8456, 77.6603),
    ("mock-blr-airport", "Kempegowda International Airport, Bengaluru, Karnataka", 13.1986, 77.7066),
]


def _matching_places(text):
    """Places whose name contains the typed text; all of them when none does,
    so the rider always has something to pick."""
    hits = [p for p in _PLACES if text and text.lower() in p[1].lower()]
    return hits or _PLACES


def _place_by_id(place_id):
    """The place with this id, or the first one for an id the mock never gave out."""
    return next((p for p in _PLACES if p[0] == place_id), _PLACES[0])


def _autocomplete(handler):
    """Classic GET place/autocomplete/json, around the request's origin or location."""
    qp = _qp(handler)
    inp = qp.get("input", [""])[0]
    centre = _parse_lat_lon(qp.get("origin", [""])[0]) or _parse_lat_lon(qp.get("location", [""])[0])
    predictions = []
    for pid, name, meters in _search(inp, centre):
        main, _, secondary = name.partition(", ")
        prediction = {
            "description": name,
            "matched_substrings": [],
            "place_id": pid,
            "reference": pid,
            "structured_formatting": {"main_text": main, "secondary_text": secondary},
            "terms": [{"offset": 0, "value": name}],
            "types": ["geocode"],
        }
        if meters is not None:
            prediction["distance_meters"] = meters
        predictions.append(prediction)
    return {"predictions": predictions, "status": "OK"}


def _autocomplete_v2(body):
    """Places API (New) POST places:autocomplete, used for the rider's pickup and drop search."""
    try:
        req = _json.loads(body or b"{}")
    except ValueError:
        req = {}
    if not isinstance(req, dict):
        req = {}
    suggestions = []
    for pid, name, meters in _search(str(req.get("input") or ""), _centre_v2(req)):
        prediction = {"placeId": pid, "text": {"text": name}, "types": ["geocode"]}
        if meters is not None:
            prediction["distanceMeters"] = meters
        suggestions.append({"placePrediction": prediction})
    return {"suggestions": suggestions}


def _place_details_v2(place_id):
    """Places API (New) GET places/{placeId}: the lat/lon of a picked place."""
    _, name, lat, lon = _place_for_id(place_id)
    return {"formattedAddress": name, "location": {"latitude": lat, "longitude": lon}, "addressComponents": []}


def _place_details(handler):
    """Classic GET place/details/json, the backend's fallback when the new API fails."""
    pid, name, lat, lon = _place_for_id(_qp(handler).get("place_id", [""])[0])
    return {
        "status": "OK",
        "result": {
            "formatted_address": name,
            "address_components": [],
            "geometry": {"location": {"lat": lat, "lng": lon}},
            "place_id": pid,
        },
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
    # No warningMessage key: the kernel's snapToRoad treats any warningMessage (even "")
    # as a failure, which fails every ride's distance calculation once OSRM match falls back here.
    return {
        "snappedPoints": [
            {"location": p, "originalIndex": i, "placeId": "mock_place_id"}
            for i, p in enumerate(pts)
        ],
    }


def _waypoint_lat_lon(waypoint):
    """(lat, lon) of a Routes API v2 waypoint ({"location": {"latLng": {...}}}), or None."""
    try:
        ll = waypoint["location"]["latLng"]
        return float(ll["latitude"]), float(ll["longitude"])
    except (KeyError, TypeError, ValueError):
        return None


def _compute_routes(body):
    """Google Routes API v2 (POST /directions/v2:computeRoutes). Response shape differs
    entirely from the classic Directions API — LTS/driver-app consume distanceMeters,
    duration and polyline.encodedPolyline. Distance and line follow the request's
    origin and destination, like _directions."""
    try:
        req = _json.loads(body or b"{}")
    except ValueError:
        req = {}
    origin = _waypoint_lat_lon(req.get("origin")) if isinstance(req, dict) else None
    destination = _waypoint_lat_lon(req.get("destination")) if isinstance(req, dict) else None
    if origin and destination:
        meters, seconds = _road_estimate(f"{origin[0]},{origin[1]}", f"{destination[0]},{destination[1]}")
        line = _straight_polyline(*origin, *destination)
    else:
        meters, seconds = 5000, 600
        line = _straight_polyline(12.97, 77.59, 12.97, 77.59)
    return {
        "routes": [
            {
                "distanceMeters": meters,
                "duration": f"{seconds}s",
                "staticDuration": f"{seconds}s",
                "polyline": {"encodedPolyline": line},
            }
        ]
    }


def handle(handler, path, body):
    if handler.command == "GET" and (path.endswith("/maps") or path.endswith("/maps/") or path.endswith("/health")):
        return handler._json({"status": "UP", "service": "mock-google"})

    # Google OAuth2 token endpoint — the Haskell mock-google served this at
    # :8019/token for the FCM client's service-account auth. Seeded
    # service-configs still point there (now a legacy port of this server).
    if "/token" in path:
        return handler._json(
            {"access_token": "mock-google-oauth-token", "expires_in": 3599, "token_type": "Bearer"}
        )

    if "computeRoutes" in path or "/directions/v2" in path:
        return handler._json(_compute_routes(body))
    if "/distancematrix" in path:
        return handler._json(_distance_matrix(handler))
    if "/directions" in path:
        return handler._json(_directions(handler))
    if ":autocomplete" in path:
        return handler._json(_autocomplete_v2(body))
    if "/places/" in path:
        return handler._json(_place_details_v2(urlparse(path).path.rstrip("/").rsplit("/", 1)[-1]))
    if "/place/details" in path:
        return handler._json(_place_details(handler))
    if "/place/autocomplete" in path:
        return handler._json(_autocomplete(handler))
    if "/geocode" in path:
        return handler._json(_geocode(handler))
    if "/snapToRoads" in path or "/roads" in path:
        return handler._json(_snap_to_road(handler))

    return handler._json({"status": "ZERO_RESULTS", "results": [], "mock": True, "path": path})
