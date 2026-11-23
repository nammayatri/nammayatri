#!/usr/bin/env python3
"""Check a company's route plan against its interest forms, before publishing it.

  python3 demand_check.py <city> <ORG> interests.json [more pages...] [--plan path/to/plan.json]

The plan defaults to plans/<city>/<ORG>.json. For every employee and every leg they asked for (TO_OFFICE morning,
TO_HOME evening) it finds their nearest stop on a route of that direction (the office end excluded) and checks:

  walk   the stop is within the employee's own maxWalkingDistanceMeters of their pickup point
  time   a departure reaches that stop (TO_OFFICE) or leaves the office (TO_HOME) within their +- timeFlexibilityMinutes
  days   the route runs on every day the employee travels

and prints the totals, the employees not covered and why, and the riders per stop. It writes
out/<city>_<ORG>_demand.geojson: employee pickup points (green covered / red not), their walking circles, the stops and the
route lines. Open it in https://geojson.io or QGIS to see the plan against the demand.
"""

import argparse
import json
import os
import sys

import geo
import interests as interest_io
import naming
import plan as plans

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "out")


def fits_on(route, at, day, wish, flex):
    """A departure that runs on that day (any day when None) is at the stop within the rider's flexibility. Each day is
    checked on its own: a rider can take 18:30 on some days and 18:45 on others."""
    return any(
        abs(naming.parse_hhmm(d["time"]) + route["offsets"][at] - wish) <= flex
        for d in route["departures"]
        if day is None or day in d["days"]
    )


def of_company(company, people):
    """(the plan company's employees, how many rows belong to other companies)."""
    mine = [p for p in people if p["company"] == company["companyName"]]
    return mine, len(people) - len(mine)


def evaluate(company, people):
    """[(person, leg, result)] where result has the nearest stop and the walk/time/days verdicts (or no route at all)."""
    stop_ids, route_stops = plans.resolve_stop_ids(company)
    results = []
    for person in people:
        for leg, wanted, wish in (("TO_OFFICE", person["toOffice"], person["pickupMin"]), ("TO_HOME", person["toHome"], person["dropMin"])):
            if not wanted:
                continue
            best = None
            for route in company["routes"]:
                if route["direction"] != leg:
                    continue
                ids = route_stops[route["routeId"]]
                # TO_OFFICE: board anywhere but the office (the last stop); TO_HOME: alight anywhere but the office (the first)
                positions = range(len(ids) - 1) if leg == "TO_OFFICE" else range(1, len(ids))
                for i in positions:
                    s = route["stops"][i]
                    d = geo.distance_m(person["homeLat"], person["homeLon"], s["lat"], s["lon"])
                    if best is None or d < best["walkM"]:
                        best = {"route": route, "pos": i, "stopId": ids[i], "walkM": d}
            if best is None:
                results.append((person, leg, {"noRoute": True}))
                continue
            route = best["route"]
            at = best["pos"] if leg == "TO_OFFICE" else 0  # TO_HOME is timed by when the shuttle leaves the office
            time_ok = wish is not None and all(fits_on(route, at, day, wish, person["flexMin"]) for day in person["days"] or [None])
            results.append(
                (
                    person,
                    leg,
                    {
                        "noRoute": False,
                        "routeId": route["routeId"],
                        "stopId": best["stopId"],
                        "walkM": round(best["walkM"]),
                        "walkOk": best["walkM"] <= person["walkM"],
                        "timeOk": time_ok,
                        "daysOk": set(person["days"]) <= set(route["serviceDays"]),
                    },
                )
            )
    return results


def covered(result):
    return not result["noRoute"] and result["walkOk"] and result["timeOk"] and result["daysOk"]


def geojson(company, people, results):
    stop_ids, route_stops = plans.resolve_stop_ids(company)
    features = []
    status = {}
    for person, leg, r in results:
        status.setdefault(person["personId"], []).append(covered(r))
    for p in people:
        ok = all(status.get(p["personId"], [False]))
        features.append(
            {
                "type": "Feature",
                "properties": {"kind": "employee", "personId": p["personId"], "name": p["name"], "covered": ok, "walkM": p["walkM"], "marker-color": "#2e7d32" if ok else "#c62828"},
                "geometry": {"type": "Point", "coordinates": [p["homeLon"], p["homeLat"]]},
            }
        )
        features.append(
            {
                "type": "Feature",
                "properties": {"kind": "walking area", "personId": p["personId"], "fill-opacity": 0.08, "stroke-width": 1},
                "geometry": {"type": "Polygon", "coordinates": [geo.circle(p["homeLat"], p["homeLon"], max(p["walkM"], 1))]},
            }
        )
    for (lat, lon), (sid, name) in stop_ids.items():
        features.append({"type": "Feature", "properties": {"kind": "stop", "stopId": sid, "name": name, "marker-symbol": "bus", "marker-color": "#1565c0"}, "geometry": {"type": "Point", "coordinates": [lon, lat]}})
    for route in company["routes"]:
        features.append(
            {
                "type": "Feature",
                "properties": {"kind": "route", "routeId": route["routeId"], "name": route["name"], "direction": route["direction"], "stroke": "#1565c0", "stroke-width": 3},
                "geometry": {"type": "LineString", "coordinates": [[s["lon"], s["lat"]] for s in route["stops"]]},
            }
        )
    return {"type": "FeatureCollection", "features": features}


def main(argv):
    ap = argparse.ArgumentParser(description="Check a company's route plan against its interest forms.")
    ap.add_argument("city")
    ap.add_argument("org")
    ap.add_argument("interests", nargs="+")
    ap.add_argument("--plan", help="plan file (default plans/<city>/<ORG>.json)")
    args = ap.parse_args(argv[1:])
    city, org = naming.check_city(args.city), naming.check_org(args.org)
    path = args.plan or os.path.join(plans.plans_dir(city), org + ".json")
    try:
        company = plans.load_plan(path)
    except (plans.PlanError, OSError) as e:
        print(f"error: {e}", file=sys.stderr)
        return 1
    if company["org"] != org:
        print(f"error: {path} is the plan of {company['org']}, not {org}", file=sys.stderr)
        return 1
    people, others = of_company(company, interest_io.load(args.interests))
    if others:
        print(f"note: left out {others} interest forms of other companies (the plan is {company['companyName']}'s)", file=sys.stderr)
    if not people:
        print(f"error: no interest forms of {company['companyName']} in {', '.join(args.interests)}", file=sys.stderr)
        return 1
    results = evaluate(company, people)

    print(f"{company['companyName']}: {len(people)} employees, plan {path}")
    for leg in ("TO_OFFICE", "TO_HOME"):
        rs = [r for _, l, r in results if l == leg]
        if not rs:
            continue
        routed = [r for r in rs if not r["noRoute"]]
        print(
            f"  {leg:<9} {sum(covered(r) for r in rs)}/{len(rs)} covered"
            f"  (walk ok {sum(r['walkOk'] for r in routed)}, time ok {sum(r['timeOk'] for r in routed)}, days ok {sum(r['daysOk'] for r in routed)}, no {leg} route {len(rs) - len(routed)})"
        )
    per_stop = {}
    for _, leg, r in results:
        if covered(r):
            per_stop[(r["routeId"], r["stopId"])] = per_stop.get((r["routeId"], r["stopId"]), 0) + 1
    if per_stop:
        print("  riders per stop (covered only):")
        for (rid, sid), count in sorted(per_stop.items()):
            print(f"    {rid:<16} {sid:<18} {count}")
    misses = [(p, leg, r) for p, leg, r in results if not covered(r)]
    if misses:
        print("  not covered:")
        for p, leg, r in misses:
            if r["noRoute"]:
                why = f"no {leg} route"
            else:
                why = ", ".join(
                    w
                    for w, bad in (
                        (f"nearest stop {r['stopId']} is {r['walkM']} m away (walks {p['walkM']} m)", not r["walkOk"]),
                        (f"no departure within +-{p['flexMin']} min", not r["timeOk"]),
                        ("route does not run on all their days", not r["daysOk"]),
                    )
                    if bad
                )
            print(f"    {p['name'] or p['personId']:<24} {leg:<9} {why}")
    os.makedirs(OUT, exist_ok=True)
    out = os.path.join(OUT, f"{city}_{org}_demand.geojson")
    with open(out, "w") as f:
        json.dump(geojson(company, people, results), f)
    print(f"map: {out}  (open in https://geojson.io)")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
