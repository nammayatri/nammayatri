"""Plan one company's shuttle routes for one day and one direction, from its interest forms (the offline planning step).

  who rides     that day's MORNING riders are TO_OFFICE + ROUND_TRIP (timed by pickup time), EVENING riders TO_HOME +
                ROUND_TRIP (timed by drop time): a round-trip employee rides both ways, a one-way employee only theirs.
  shared stops  greedy cover over the employees' own pickup points: repeatedly take the point the most still-uncovered
                employees can walk to within their own max walking distance, while it serves >= min_riders.
  routes        stops swept by bearing around the office into routes of one side of town (<= max_stops, <= 70 degrees;
                riders placed later never push a route past max_stops pickups: a full route gets no new stop),
                picking up farthest first and ending at the office. Trips stay short: a route estimated over 30 minutes is
                split into an express from its far stops and a route for its near stops.
  nobody missed in this order: a point a route passes within their walking distance; a meeting point several can walk
                to, on a route if the detour is small (<= 2.5 km, route <= 40 min), else its own shuttle when enough people
                are there; their own pickup point if the detour is small; else the nearest point of the nearest route,
                which they walk to.
  roads         stops snap to the nearest road (OSRM, within 200 m) and each route is driven along the roads, one-way
                rules respected; legs without road data are straight-line estimates.
  departures    15-minute slots fitting the most riders within their own flexibility, plus a slot for anyone left.
  EVENING       the same layout reversed: from the office, then the drops.
  stop names    the public road a stop is on (OSRM), else "Pickup point": never an employee's own address.

Each route belongs to one company, as the naming convention (naming.py) has it: CORP-<ORG>-<n>.
"""

import geo
import osrm

TARGET_ROUTE_MIN = 30
MAX_ROUTE_MIN = 40
MAX_DETOUR_M = 2500
MERGE_STOP_M = 150
MAX_SECTOR_GAP_DEG = 60
MAX_SECTOR_SPAN_DEG = 70
SLOT_MIN = 15
SNAP_MAX_M = 200


def _d(a, b):
    return geo.distance_m(a["lat"], a["lon"], b["lat"], b["lon"])


def riders_for(people, direction, day):
    """That day's riders in that direction, each with the time they want ('wish', minutes after midnight)."""
    out = []
    for p in people:
        wants = p["toOffice"] if direction == "MORNING" else p["toHome"]
        wish = p["pickupMin"] if direction == "MORNING" else p["dropMin"]
        if wants and day in p["days"] and wish is not None:
            out.append({**p, "home": {"lat": p["homeLat"], "lon": p["homeLon"]}, "wish": wish})
    return sorted(out, key=lambda r: r["personId"])


def _cover(candidates, riders, min_riders):
    """Greedy max-coverage: each pick is the candidate point the most remaining riders can walk to."""
    left, picked = list(riders), []
    while left:
        best = None
        for c in candidates:
            reach = [r for r in left if _d(c["at"], r["home"]) <= r["walkM"]]
            if len(reach) > (len(best["riders"]) if best else 0):
                best = {**c, "riders": reach}
        if not best or len(best["riders"]) < min_riders:
            break
        picked.append(best)
        ids = {r["personId"] for r in best["riders"]}
        left = [r for r in left if r["personId"] not in ids]
    return picked, left


def _sweep(points, hub, max_stops):
    """Cut stops into routes by bearing around the hub, one side of town each."""
    pts = sorted(points, key=lambda p: geo.bearing_deg(hub, p))
    if len(pts) > 1:
        gaps = [(geo.bearing_deg(hub, pts[i]) - geo.bearing_deg(hub, pts[i - 1])) % 360 for i in range(len(pts))]
        start = max(range(len(pts)), key=lambda i: gaps[i])
        pts = pts[start:] + pts[:start]
    groups = []
    for i, p in enumerate(pts):
        gap = 0 if i == 0 else (geo.bearing_deg(hub, p) - geo.bearing_deg(hub, pts[i - 1])) % 360
        span = (geo.bearing_deg(hub, p) - geo.bearing_deg(hub, groups[-1][0])) % 360 if groups else 0
        if not groups or len(groups[-1]) >= max_stops or gap > MAX_SECTOR_GAP_DEG or span > MAX_SECTOR_SPAN_DEG:
            groups.append([p])
        else:
            groups[-1].append(p)
    return groups


def _stop(at, kind, rider_ids):
    return {"lat": at["lat"], "lon": at["lon"], "kind": kind, "riders": list(rider_ids)}


def _pickups(route):
    return sum(1 for s in route if s["kind"] != "OFFICE")


def _make_route(pickups, office):
    ordered = sorted(pickups, key=lambda s: -_d(s, office))  # farthest first: heads steadily in to the office
    return ordered + [_stop(office, "OFFICE", [])]


def _minutes(stops):
    return geo.estimate_route_minutes(stops)


def _split_long(stops, office):
    pickups = [s for s in stops if s["kind"] != "OFFICE"]
    if len(pickups) < 2 or _minutes(stops) <= TARGET_ROUTE_MIN:
        return [stops]
    half = (len(pickups) + 1) // 2
    return _split_long(_make_route(pickups[:half], office), office) + _split_long(_make_route(pickups[half:], office), office)


def _insert_pickup(routes, stop, max_stops):
    """Add the pickup where it costs the least detour, if that stays <= MAX_DETOUR_M, the route <= MAX_ROUTE_MIN and the
    route has room (< max_stops pickups)."""
    best = None
    for route in routes:
        if _pickups(route) >= max_stops:
            continue
        index, added = geo.cheapest_insertion(route, stop, 0, len(route) - 1)
        trial = route[:index] + [stop] + route[index:]
        if added <= MAX_DETOUR_M and _minutes(trial) <= MAX_ROUTE_MIN and (best is None or added < best[0]):
            best = (added, route, index)
    if not best:
        return False
    _, route, index = best
    route.insert(index, stop)
    return True


def _near_stop(route, point):
    return next((s for s in route if s["kind"] != "OFFICE" and _d(s, point) <= MERGE_STOP_M), None)


def _place_on_route(routes, rider, max_stops, any_distance=False):
    """Board the rider at a point a route passes: within their walk (ON_ROUTE), or with any_distance the nearest point
    of the nearest route however far (WALK_TO_ROUTE). A stop close by is reused; a new stop goes only on a route with
    room (< max_stops pickups)."""
    best = None
    for route in routes:
        full = _pickups(route) >= max_stops
        for i in range(len(route) - 1):
            point, meters = geo.project_onto_segment(rider["home"], route[i], route[i + 1])
            if full and _near_stop(route, point) is None:
                continue
            if (any_distance or meters <= rider["walkM"]) and (best is None or meters < best[0]):
                best = (meters, route, i + 1, point)
    if not best:
        return False
    meters, route, index, point = best
    near = _near_stop(route, point)
    if near:
        near["riders"].append(rider["personId"])
    elif meters <= rider["walkM"]:
        route.insert(index, _stop(point, "ON_ROUTE", [rider["personId"]]))
    else:
        route.insert(index, _stop(point, "WALK_TO_ROUTE", [rider["personId"]]))
    return True


def layout(riders, office, min_riders, max_stops):
    """Routes (stop lists in MORNING order: pickups, then the office) that carry every rider."""
    homes = [{"at": r["home"]} for r in riders]
    shared, left = _cover(homes, riders, min_riders)
    if not shared and riders:
        shared, left = _cover(homes, riders, 1)
    routes = []
    for group in _sweep([{**p["at"], "p": p} for p in shared], office, max_stops):
        pickups = [_stop(g["p"]["at"], "SHARED", [r["personId"] for r in g["p"]["riders"]]) for g in group]
        routes += _split_long(_make_route(pickups, office), office)
    left = [r for r in left if not _place_on_route(routes, r, max_stops)]
    pairs = [{"at": {"lat": (a["home"]["lat"] + b["home"]["lat"]) / 2, "lon": (a["home"]["lon"] + b["home"]["lon"]) / 2}} for i, a in enumerate(left) for b in left[i + 1:]]
    meeting, walkers = _cover([{"at": r["home"]} for r in left] + pairs, left, 2)
    for m in meeting:
        stop = _stop(m["at"], "MEETING_POINT", [r["personId"] for r in m["riders"]])
        if _insert_pickup(routes, stop, max_stops):
            continue
        own = _make_route([stop], office)
        if len(m["riders"]) >= min_riders and _minutes(own) <= MAX_ROUTE_MIN:
            routes.append(own)
        else:
            walkers += m["riders"]
    for r in walkers:
        doorstep = _stop(r["home"], "DOORSTEP", [r["personId"]])
        # a full route takes no new stop: the rider then gets a shuttle of their own, never dropped
        if not _insert_pickup(routes, doorstep, max_stops) and not _place_on_route(routes, r, max_stops, any_distance=True):
            routes.append(_make_route([doorstep], office))
    return routes


def on_roads(stops):
    """Snap stops to roads and drive them along the roads: (stops on the map, leg seconds, shape [[lat, lon]...])."""
    snapped = []
    for s in stops:
        sn = osrm.snap(s)
        ok = sn is not None and sn["meters"] <= SNAP_MAX_M
        snapped.append({**s, "lat": sn["lat"] if ok else s["lat"], "lon": sn["lon"] if ok else s["lon"], "street": sn["street"] if ok else "", "onRoad": ok})
    legs = [geo.estimate_leg(snapped[i], snapped[i + 1])[1] for i in range(len(snapped) - 1)]
    shape = [[snapped[0]["lat"], snapped[0]["lon"]]]
    i = 0
    while i < len(snapped) - 1:
        j = i
        while j + 1 < len(snapped) and snapped[j]["onRoad"] and snapped[j + 1]["onRoad"]:
            j += 1
        road = osrm.route(snapped[i:j + 1]) if j > i else None
        if road:
            legs[i:j] = road["legSeconds"]
            shape += road["geometry"][1:]
            i = j
        else:
            shape.append([snapped[i + 1]["lat"], snapped[i + 1]["lon"]])
            i += 1
    return snapped, legs, shape


def _candidate_slots(wishes):
    """Every 15-minute slot inside some rider's window (wish +- flexibility), plus each wish's nearest slot."""
    slots = set()
    for wish, flex in wishes:
        slots.add(round(wish / SLOT_MIN) * SLOT_MIN)
        first, last = -(-(wish - flex) // SLOT_MIN), (wish + flex) // SLOT_MIN
        slots.update(k * SLOT_MIN for k in range(int(first), int(last) + 1))
    return sorted(slots)


def pick_departures(wishes, min_riders):
    """15-minute slots fitting the most wishes (within each rider's flexibility), then a slot for anyone left."""
    remaining, slots, floor = list(wishes), [], max(1, min_riders)
    while remaining:
        best, fit, cost = None, [], None
        for slot in _candidate_slots(remaining):
            f = [x for x in remaining if abs(slot - x[0]) <= x[1]]
            c = sum(abs(slot - x[0]) for x in f)  # among equal fits, the slot closest to what the riders asked for
            if len(f) > len(fit) or (f and len(f) == len(fit) and c < cost):
                best, fit, cost = slot, f, c
        if not fit:
            slots.append(round(remaining[0][0]))
            remaining = remaining[1:]
            continue
        if len(fit) < floor:
            floor = 1
            continue
        slots.append(best)
        remaining = [x for x in remaining if x not in fit]
    return sorted(set(slots))


def _label(stop, company):
    if stop["kind"] == "OFFICE":
        return f"{company} office"
    if stop["kind"] == "WALK_TO_ROUTE":
        return f"Walk-up point, {stop['street']}" if stop.get("street") else "Walk-up point"
    # the public road the stop is on; never the employee's address, which is not published
    return stop.get("street") or "Pickup point"


def plan_day(people, company, office, direction, day, min_riders=2, max_stops=6):
    """The company's routes for one day and direction: [{stops, offsets, departures, shape, riders}]."""
    riders = riders_for(people, direction, day)
    by_id = {r["personId"]: r for r in riders}
    out = []
    for stops in layout(riders, office, min_riders, max_stops):
        if direction == "EVENING":
            stops = list(reversed(stops))
        on_map, legs, shape = on_roads(stops)
        offsets = [0]
        for sec in legs:
            offsets.append(offsets[-1] + max(1, round(sec / 60)) + geo.DWELL_MIN)
        office_at = next(i for i, s in enumerate(on_map) if s["kind"] == "OFFICE")
        boardings = [(by_id[pid], i) for i, s in enumerate(on_map) for pid in s["riders"]]
        wishes = [(r["wish"] - offsets[i if direction == "MORNING" else office_at], r["flexMin"]) for r, i in boardings]
        departures = pick_departures(wishes, min_riders)
        outcomes = []
        for (r, i), (want, _) in zip(boardings, wishes):
            dep = min(departures, key=lambda d: abs(d - want))
            walk = round(_d(r["home"], on_map[i]))
            kind = on_map[i]["kind"]
            outcomes.append({
                "personId": r["personId"], "name": r["name"], "kind": "WALK_TO_ROUTE" if kind != "DOORSTEP" and walk > r["walkM"] else kind,
                "walkM": walk, "walkLimitM": r["walkM"], "departure": dep, "stopTime": dep + offsets[i], "stop": _label(on_map[i], company),
            })
        out.append({
            "stops": [{"name": _label(s, company), "lat": round(s["lat"], 6), "lon": round(s["lon"], 6), "kind": s["kind"]} for s in on_map],
            "offsets": offsets, "departures": departures, "shape": [[round(a, 6), round(b, 6)] for a, b in shape], "riders": outcomes,
        })
    return out
