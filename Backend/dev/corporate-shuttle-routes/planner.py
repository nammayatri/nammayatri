"""Plan one company's shuttle routes for one direction, from its interest forms (the offline planning step).

  who rides     MORNING riders are TO_OFFICE + ROUND_TRIP (timed by pickup time), EVENING riders TO_HOME + ROUND_TRIP
                (timed by drop time): a round-trip employee rides both ways, a one-way employee only theirs.
  pickup points everyone within WALK_M (500 m) of a point shares it, whatever their time: greedy cover over the
                employees' own pickup points, the point the most still-unplaced employees can walk to first.
  routes        pickup points within LINK_M (3 km) of each other, directly or through other points, are one route, so
                nearby stops never end up on different routes. A route picks up farthest first and ends at the office.
                The layout is planned once from every day's riders, so a route has the same stops on every day.
  shuttles      per route and day, the riders who asked for the same time (to 15 minutes) share a shuttle. A shuttle
                seats SEATS (8) and should carry at least MIN_LOAD (5): a full group is split into shuttles
                SPLIT_GAP_MIN apart, then a light group joins the nearest-time group of its route within MERGE_MIN
                when both fit in one shuttle. Nobody is dropped: a shuttle that stays light still runs.
                A MORNING shuttle leaves so it is at its first riders' stop at their time; an EVENING one leaves the
                office at its riders' time.
  roads         stops snap to the nearest road (OSRM, within 200 m) and each route is driven along the roads, one-way
                rules respected; legs without road data are straight-line estimates.
  EVENING       the same layout reversed: from the office, then the drops.
  stop names    the public road a stop is on (OSRM), else "Pickup point": never an employee's own address.

Each route belongs to one company, as the naming convention (naming.py) has it: CORP-<ORG>-<n>.
"""

import geo
import naming
import osrm

WALK_M = 500
LINK_M = 3000
SLOT_MIN = 15
SEATS = 8
MIN_LOAD = 5
MERGE_MIN = 30
SPLIT_GAP_MIN = 5
SNAP_MAX_M = 200


def _d(a, b):
    return geo.distance_m(a["lat"], a["lon"], b["lat"], b["lon"])


def riders_for(people, direction, day=None):
    """The riders in that direction (on that day, or on any day), each with the time they want ('wish', minutes after
    midnight)."""
    out = []
    for p in people:
        wants = p["toOffice"] if direction == "MORNING" else p["toHome"]
        wish = p["pickupMin"] if direction == "MORNING" else p["dropMin"]
        if wants and wish is not None and (day in p["days"] if day else p["days"]):
            out.append({**p, "home": {"lat": p["homeLat"], "lon": p["homeLon"]}, "wish": wish})
    return sorted(out, key=lambda r: r["personId"])


def _cover(riders):
    """Greedy cover: each pick is the rider's pickup point the most still-unplaced riders are within WALK_M of."""
    left, picked = list(riders), []
    while left:
        best = None
        for c in riders:
            reach = [r for r in left if _d(c["home"], r["home"]) <= WALK_M]
            if len(reach) > (len(best["riders"]) if best else 0):
                best = {"at": c["home"], "riders": reach}
        picked.append(best)
        ids = {r["personId"] for r in best["riders"]}
        left = [r for r in left if r["personId"] not in ids]
    return picked


def _link(stops):
    """Stops within LINK_M of each other, directly or through other stops, form one group (one route)."""
    groups = []
    for s in stops:
        near = [g for g in groups if any(_d(s, t) <= LINK_M for t in g)]
        groups = [g for g in groups if not any(g is n for n in near)] + [[t for g in near for t in g] + [s]]
    return groups


def _stop(at, kind, rider_ids):
    return {"lat": at["lat"], "lon": at["lon"], "kind": kind, "riders": list(rider_ids)}


def _make_route(pickups, office):
    ordered = sorted(pickups, key=lambda s: -_d(s, office))  # farthest first: heads steadily in to the office
    return ordered + [_stop(office, "OFFICE", [])]


def layout(riders, office):
    """Routes (stop lists in MORNING order: pickups, then the office) that carry every rider."""
    stops = [_stop(p["at"], "SHARED", [r["personId"] for r in p["riders"]]) for p in _cover(riders)]
    return [_make_route(group, office) for group in _link(stops)]


def shuttles(wishes):
    """One route's shuttles on one day: [(time, [rider ids])] from (wanted time, rider id) pairs, by SEATS/MIN_LOAD."""
    slots = {}
    for want, pid in sorted(wishes):
        slots.setdefault(round(want / SLOT_MIN) * SLOT_MIN, []).append(pid)
    groups = []
    for t, ids in sorted(slots.items()):
        n = -(-len(ids) // SEATS)
        size = -(-len(ids) // n)
        groups += [(t + k * SPLIT_GAP_MIN, ids[k * size:(k + 1) * size]) for k in range(n)]
    merged = True
    while merged:
        merged = False
        for i, (t, ids) in enumerate(groups):
            near = [j for j in (i - 1, i + 1) if 0 <= j < len(groups) and abs(groups[j][0] - t) <= MERGE_MIN and len(groups[j][1]) + len(ids) <= SEATS]
            if len(ids) < MIN_LOAD and near:
                a, b = sorted((i, min(near, key=lambda j: abs(groups[j][0] - t))))
                (ta, ia), (tb, ib) = groups[a], groups[b]
                groups[a:b + 1] = [(ta if len(ia) >= len(ib) else tb, ia + ib)]
                merged = True
                break
    return groups


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


def _label(stop, company):
    if stop["kind"] == "OFFICE":
        return f"{company} office"
    # the public road the stop is on; never the employee's address, which is not published
    return stop.get("street") or "Pickup point"


def plan_direction(people, company, office, direction):
    """The company's routes in one direction, the same stops every day: [{stops, offsets, shape, trips}], trips being
    {day: [(departure, [rider ids])]}."""
    out = []
    for stops in layout(riders_for(people, direction), office):
        if direction == "EVENING":
            stops = list(reversed(stops))
        on_map, legs, shape = on_roads(stops)
        offsets = [0]
        for sec in legs:
            offsets.append(offsets[-1] + max(1, round(sec / 60)) + geo.DWELL_MIN)
        at = {pid: i for i, s in enumerate(on_map) for pid in s["riders"]}
        trips = {}
        for day in naming.DAYS:
            groups = shuttles([(r["wish"], r["personId"]) for r in riders_for(people, direction, day) if r["personId"] in at])
            used = set()
            for t, ids in groups:
                # MORNING: at the first stop with riders of this shuttle at their time; EVENING: from the office then
                dep = t - min(offsets[at[pid]] for pid in ids) if direction == "MORNING" else t
                while dep in used:
                    dep += SPLIT_GAP_MIN
                used.add(dep)
                trips.setdefault(day, []).append((dep, ids))
        out.append({
            "stops": [{"name": _label(s, company), "lat": round(s["lat"], 6), "lon": round(s["lon"], 6), "kind": s["kind"]} for s in on_map],
            "offsets": offsets, "shape": [[round(a, 6), round(b, 6)] for a, b in shape], "trips": trips,
        })
    return out
