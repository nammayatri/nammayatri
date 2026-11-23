"""Small geometry helpers (no dependencies)."""

import math

EARTH_M = 6371008.8
ROAD_FACTOR = 1.4  # straight line -> road distance, as the shared cab's fare estimate uses
SPEED_KMPH = 20.0  # city peak-hour shuttle speed for draft stop times; the planner adjusts them
DWELL_MIN = 1


def distance_m(lat1, lon1, lat2, lon2):
    p1, p2 = math.radians(lat1), math.radians(lat2)
    dp, dl = p2 - p1, math.radians(lon2 - lon1)
    a = math.sin(dp / 2) ** 2 + math.cos(p1) * math.cos(p2) * math.sin(dl / 2) ** 2
    return 2 * EARTH_M * math.asin(math.sqrt(a))


def drive_minutes(lat1, lon1, lat2, lon2):
    """Draft driving time between consecutive stops, including the dwell at the stop; at least 2 minutes."""
    km = distance_m(lat1, lon1, lat2, lon2) * ROAD_FACTOR / 1000
    return max(2, math.ceil(km / SPEED_KMPH * 60) + DWELL_MIN)


def circle(lat, lon, radius_m, points=32):
    """A closed GeoJSON ring approximating a circle (for walking-distance areas)."""
    ring = []
    for i in range(points + 1):
        a = 2 * math.pi * (i % points) / points
        dlat = radius_m * math.cos(a) / EARTH_M
        dlon = radius_m * math.sin(a) / (EARTH_M * math.cos(math.radians(lat)))
        ring.append([round(lon + math.degrees(dlon), 6), round(lat + math.degrees(dlat), 6)])
    return ring


def median(values):
    s = sorted(values)
    n = len(s)
    return s[n // 2] if n % 2 else (s[n // 2 - 1] + s[n // 2]) / 2


def estimate_leg(a, b):
    """Straight-line leg estimate (no OSRM): (meters, seconds) at ROAD_FACTOR and SPEED_KMPH."""
    meters = distance_m(a["lat"], a["lon"], b["lat"], b["lon"]) * ROAD_FACTOR
    return meters, meters / 1000 / SPEED_KMPH * 3600


def estimate_route_minutes(points):
    """Minutes to drive the points in order, 1 minute at each stop after the first (straight-line estimate)."""
    return sum(estimate_leg(points[i], points[i + 1])[1] / 60 + DWELL_MIN for i in range(len(points) - 1))


def bearing_deg(frm, to):
    p1, p2 = math.radians(frm["lat"]), math.radians(to["lat"])
    dl = math.radians(to["lon"] - frm["lon"])
    y = math.sin(dl) * math.cos(p2)
    x = math.cos(p1) * math.sin(p2) - math.sin(p1) * math.cos(p2) * math.cos(dl)
    return (math.degrees(math.atan2(y, x)) + 360) % 360


def project_onto_segment(p, a, b):
    """The point of segment a-b nearest to p (local flat projection, fine at city scale), and its distance in metres."""
    k = math.cos(math.radians(p["lat"]))
    ax, ay, bx, by, px, py = a["lon"] * k, a["lat"], b["lon"] * k, b["lat"], p["lon"] * k, p["lat"]
    len2 = (bx - ax) ** 2 + (by - ay) ** 2
    t = 0 if len2 == 0 else max(0.0, min(1.0, ((px - ax) * (bx - ax) + (py - ay) * (by - ay)) / len2))
    point = {"lat": ay + t * (by - ay), "lon": (ax + t * (bx - ax)) / k}
    return point, distance_m(p["lat"], p["lon"], point["lat"], point["lon"])


def cheapest_insertion(seq, p, lo, hi):
    """Where inserting p into seq adds the least distance: (index to insert before, added metres), index in [lo, hi]."""
    d = lambda a, b: distance_m(a["lat"], a["lon"], b["lat"], b["lon"])
    best = (lo, float("inf"))
    for i in range(max(lo, 0), min(hi, len(seq)) + 1):
        prev = seq[i - 1] if i > 0 else None
        nxt = seq[i] if i < len(seq) else None
        if prev and nxt:
            added = d(prev, p) + d(p, nxt) - d(prev, nxt)
        else:
            added = d(prev, p) if prev else (d(p, nxt) if nxt else 0.0)
        if added < best[1]:
            best = (i, added)
    return best
