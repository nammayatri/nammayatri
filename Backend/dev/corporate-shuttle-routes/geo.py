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


def median(values):
    s = sorted(values)
    n = len(s)
    return s[n // 2] if n % 2 else (s[n // 2 - 1] + s[n // 2]) / 2


def estimate_leg(a, b):
    """Straight-line leg estimate (no OSRM): (meters, seconds) at ROAD_FACTOR and SPEED_KMPH."""
    meters = distance_m(a["lat"], a["lon"], b["lat"], b["lon"]) * ROAD_FACTOR
    return meters, meters / 1000 / SPEED_KMPH * 3600
