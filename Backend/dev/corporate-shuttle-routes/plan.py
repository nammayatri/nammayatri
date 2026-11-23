"""Company route plans: load, check, and resolve them into the ids of the naming convention.

A plan is plans/<city>/<ORG>.json, one per company:

{
  "org": "ACME",                       # company code used in every id (upper-case letters/digits)
  "companyName": "ACME Corp",
  "routes": [
    {
      "n": 1,                          # route CORP-ACME-1
      "name": "Sholinganallur - ACME campus",
      "direction": "TO_OFFICE",        # TO_OFFICE (ends at the office) or TO_HOME (starts at the office)
      "stops": [{"name": "...", "lat": 12.9, "lon": 80.2}, ...],          # in driving order
      "minutesBetweenStops": 10,       # or "stopOffsetsMinutes": [0, 8, 15, ...] (first is 0)
      "departures": ["08:30", "18:00"],# from the first stop; or [{"time": "08:30", "days": ["monday", ...],
                                       # "riders": {"monday": 4, ...}}] when the days differ by departure (within
                                       # serviceDays); riders is optional: the planned employees per day
      "serviceDays": ["monday", "tuesday", "wednesday", "thursday", "friday"],
      "startDate": "20270101", "endDate": "20271231",
      "fareInr": 40,                   # flat seat fare for every forward stop pair
      "pairFaresInr": [{"from": 1, "to": 3, "amount": 30}],  # optional overrides (1-based stop positions)
      "shape": [[12.9, 77.6], ...]     # optional: the route along the roads (written as the GTFS shape)
    }
  ]
}
"""

import datetime
import json
import os
import re

import naming

HERE = os.path.dirname(os.path.abspath(__file__))
DATE_RE = re.compile(r"^[0-9]{8}$")
DIRECTIONS = ("TO_OFFICE", "TO_HOME")


class PlanError(ValueError):
    pass


def plans_dir(city):
    return os.path.join(HERE, "plans", naming.check_city(city))


def load_city_config(city):
    path = os.path.join(HERE, "cities", naming.check_city(city) + ".json")
    if not os.path.exists(path):
        raise PlanError(f"no city config {path}")
    with open(path) as f:
        config = json.load(f)
    for key in ("merchantId", "merchantOperatingCityId", "integratedBppConfigId", "vehicleServiceTierId", "vehicleType", "timezone", "cityName", "idSeedPrefix"):
        if not config.get(key):
            raise PlanError(f"{path}: missing {key}")
    return config


def load_city_plans(city):
    """Every company plan of the city, sorted by org code."""
    directory = plans_dir(city)
    if not os.path.isdir(directory):
        raise PlanError(f"no plans in {directory}")
    plans = []
    for name in sorted(os.listdir(directory)):
        if name.endswith(".json"):
            plans.append(load_plan(os.path.join(directory, name)))
    if not plans:
        raise PlanError(f"no plans in {directory}")
    orgs = [p["org"] for p in plans]
    duplicate = sorted({o for o in orgs if orgs.count(o) > 1})
    if duplicate:
        raise PlanError(f"org code used by more than one plan in {directory}: {', '.join(duplicate)}")
    return plans


def load_plan(path):
    with open(path) as f:
        raw = json.load(f)
    try:
        return check_plan(raw)
    except (PlanError, ValueError) as e:
        raise PlanError(f"{path}: {e}") from None


def _is_date(text):
    """A real calendar date written YYYYMMDD (20270230 is not one)."""
    if not DATE_RE.match(text):
        return False
    try:
        datetime.datetime.strptime(text, "%Y%m%d")
    except ValueError:
        return False
    return True


def _number(value, what):
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise PlanError(f"{what} must be a number: {value!r}")
    return float(value)


def check_plan(raw):
    org = naming.check_org(raw.get("org"))
    routes = raw.get("routes") or []
    if not routes:
        raise PlanError("a plan needs at least one route")
    numbers = [r.get("n") for r in routes]
    if len(set(numbers)) != len(numbers):
        raise PlanError(f"route numbers repeat: {numbers}")
    checked = [check_route(org, r) for r in sorted(routes, key=lambda r: r.get("n") or 0)]
    return {"org": org, "companyName": raw.get("companyName") or org, "routes": checked}


def check_route(org, r):
    rid = naming.route_id(org, r.get("n"))
    where = rid
    direction = r.get("direction", "TO_OFFICE")
    if direction not in DIRECTIONS:
        raise PlanError(f"{where}: direction must be one of {DIRECTIONS}")
    stops = r.get("stops") or []
    if len(stops) < 2:
        raise PlanError(f"{where}: a route needs at least 2 stops")
    clean_stops = []
    for i, s in enumerate(stops, 1):
        name = (s.get("name") or "").strip()
        if not name:
            raise PlanError(f"{where}: stop {i} has no name")
        lat, lon = _number(s.get("lat"), f"{where} stop {i} lat"), _number(s.get("lon"), f"{where} stop {i} lon")
        if not (-90 <= lat <= 90 and -180 <= lon <= 180):
            raise PlanError(f"{where}: stop {i} ({name}) has coordinates out of range")
        clean_stops.append({"name": name, "lat": round(lat, 6), "lon": round(lon, 6)})
    keys = [(s["lat"], s["lon"]) for s in clean_stops]
    if len(set(keys)) != len(keys):
        raise PlanError(f"{where}: the same stop appears twice in one route")

    if "stopOffsetsMinutes" in r:
        offsets = r["stopOffsetsMinutes"]
        if not isinstance(offsets, list) or len(offsets) != len(clean_stops) or any(not isinstance(o, int) or isinstance(o, bool) for o in offsets):
            raise PlanError(f"{where}: stopOffsetsMinutes needs one whole number per stop")
    elif "minutesBetweenStops" in r:
        gap = r["minutesBetweenStops"]
        if not isinstance(gap, int) or isinstance(gap, bool) or gap < 1:
            raise PlanError(f"{where}: minutesBetweenStops must be a whole number >= 1")
        offsets = [i * gap for i in range(len(clean_stops))]
    else:
        raise PlanError(f"{where}: give minutesBetweenStops or stopOffsetsMinutes")
    if offsets[0] != 0 or any(b <= a for a, b in zip(offsets, offsets[1:])):
        raise PlanError(f"{where}: stop times must start at 0 and strictly increase: {offsets}")

    days = [str(d).lower() for d in (r.get("serviceDays") or [])]
    bad = [d for d in days if d not in naming.DAYS]
    if bad or not days:
        raise PlanError(f"{where}: serviceDays must be day names (monday..sunday), got {r.get('serviceDays')!r}")

    raw_departures = r.get("departures") or []
    if not raw_departures:
        raise PlanError(f"{where}: a route needs at least one departure")
    departures = []
    for d in raw_departures:
        time, dep_days = (d, days) if isinstance(d, str) else (d.get("time"), [str(x).lower() for x in d.get("days") or []])
        naming.parse_hhmm(time)
        if not dep_days or any(x not in days for x in dep_days):
            raise PlanError(f"{where}: departure {time} must run on some of the route's serviceDays")
        riders = {} if isinstance(d, str) else {str(k).lower(): v for k, v in (d.get("riders") or {}).items()}
        if any(k not in dep_days or not isinstance(v, int) or isinstance(v, bool) or v < 0 for k, v in riders.items()):
            raise PlanError(f"{where}: departure {time} riders must be whole numbers for its own days")
        departures.append({"time": time, "days": [x for x in naming.DAYS if x in dep_days], "riders": riders})
    minutes = [naming.parse_hhmm(d["time"]) for d in departures]
    if len(set(minutes)) != len(minutes):
        raise PlanError(f"{where}: departures repeat: {[d['time'] for d in departures]}")

    shape = r.get("shape")
    if shape is not None and (not isinstance(shape, list) or len(shape) < 2 or any(not isinstance(p, list) or len(p) != 2 for p in shape)):
        raise PlanError(f"{where}: shape must be a list of [lat, lon] points")
    for i, (lat, lon) in enumerate(shape or [], 1):
        lat, lon = _number(lat, f"{where} shape point {i} lat"), _number(lon, f"{where} shape point {i} lon")
        if not (-90 <= lat <= 90 and -180 <= lon <= 180):
            raise PlanError(f"{where}: shape point {i} has coordinates out of range ({lat}, {lon})")
    start, end = str(r.get("startDate", "")), str(r.get("endDate", ""))
    if not (_is_date(start) and _is_date(end)) or end < start:
        raise PlanError(f"{where}: startDate/endDate must be real YYYYMMDD dates with endDate >= startDate")

    fare = _number(r.get("fareInr"), f"{where} fareInr")
    if fare <= 0:
        raise PlanError(f"{where}: fareInr must be greater than 0")
    pair_fares = {}
    for p in r.get("pairFaresInr") or []:
        a, b, amount = p.get("from"), p.get("to"), _number(p.get("amount"), f"{where} pair fare")
        if not (isinstance(a, int) and isinstance(b, int) and 1 <= a < b <= len(clean_stops)):
            raise PlanError(f"{where}: pair fare from/to must be stop positions with from < to: {p!r}")
        if amount <= 0:
            raise PlanError(f"{where}: pair fare amount must be greater than 0: {p!r}")
        pair_fares[(a, b)] = amount

    return {
        "n": r["n"],
        "routeId": rid,
        "name": (r.get("name") or rid).strip(),
        "direction": direction,
        "stops": clean_stops,
        "offsets": offsets,
        "departures": sorted(departures, key=lambda d: naming.parse_hhmm(d["time"])),
        "shape": [[float(a), float(b)] for a, b in shape] if shape else None,
        "serviceDays": [d for d in naming.DAYS if d in days],
        "startDate": start,
        "endDate": end,
        "fareInr": fare,
        "pairFaresInr": pair_fares,
    }


def resolve_stop_ids(plan):
    """Number the company's stops CORP_<ORG>_S1.. in order of first use across its routes (a stop shared by two routes,
    such as the office, gets one id). Returns {(lat, lon): (stop_id, name)} and each route's stop ids."""
    ids = {}
    route_stops = {}
    for route in plan["routes"]:
        seq = []
        for s in route["stops"]:
            key = (s["lat"], s["lon"])
            if key not in ids:
                ids[key] = (naming.stop_id(plan["org"], len(ids) + 1), s["name"])
            seq.append(ids[key][0])
        route_stops[route["routeId"]] = seq
    return ids, route_stops


def fare_of(route, i, j):
    """Seat fare between stop positions i < j (1-based)."""
    return route["pairFaresInr"].get((i, j), route["fareInr"])
