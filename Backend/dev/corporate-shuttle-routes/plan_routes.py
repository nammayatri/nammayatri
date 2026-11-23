#!/usr/bin/env python3
"""Plan every partner company's shuttle routes from its interest forms (the offline planning step).

  python3 plan_routes.py <city> --from-api                    # read the forms from the local dashboard API
  python3 plan_routes.py <city> interests.json [page2.json]   # or from saved shuttleInterest/list responses
        [--start YYYYMMDD] [--end YYYYMMDD] [--fare 40] [--min-riders 2] [--max-stops 6]

For each company, direction (MORNING to the office, EVENING home) and day of the week, planner.plan_day lays out routes
that carry every rider along the roads (see planner.py). A route with the same stops on several days is one route: its
departures carry the days they run. Writes plans/<city>/<ORG>.json, one per company, in the format build_feed.py reads
(naming.py: CORP-<ORG>-<n>), and out/<city>_plan_report.txt with each day's coverage and the employees who need
attention (a doorstep detour, or walking further than they said).

Next: python3 build_feed.py <city> && python3 validate.py <city>, then publish (README.md).
"""

import argparse
import datetime
import json
import os
import sys
import urllib.request

import geo
import interests as interest_io
import naming
import plan as plans
import planner

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "out")
DIRECTIONS = ("MORNING", "EVENING")


def org_code(company, config):
    """The company's code in every id: from cities/<city>.json "orgs", else its initials ("Namma Yatri" -> "NY")."""
    if company in config.get("orgs", {}):
        return naming.check_org(config["orgs"][company])
    words = [w for w in "".join(c if c.isalnum() else " " for c in company.split(".")[0]).split() if w]
    return naming.check_org(("".join(w[0] for w in words) if len(words) > 1 else (words[0] if words else "CO")).upper()[:8])


def fetch_interests(config):
    """Every interest form of the city from the local dashboard API (login with the local dev dashboard user)."""
    rider = os.environ.get("RIDER_APP_URL", "http://localhost:8013")
    login_url = os.environ.get("DASHBOARD_LOGIN_URL", "http://localhost:8016") + "/direct-dashboard/user/login"
    creds = {"email": os.environ.get("LOCAL_DASHBOARD_EMAIL", "juspay_admin@dashboard.com"), "password": os.environ.get("LOCAL_DASHBOARD_PASSWORD", "juspay_admin")}
    req = urllib.request.Request(login_url, data=json.dumps(creds).encode(), headers={"content-type": "application/json"})
    with urllib.request.urlopen(req, timeout=15) as resp:
        token = json.load(resp)["authToken"]
    rows, offset = [], 0
    while True:
        url = f"{rider}/direct-dashboard/{config['merchantShortId']}/{config['apiCity']}/shuttleInterest/list?limit=100&offset={offset}"
        with urllib.request.urlopen(urllib.request.Request(url, headers={"token": token}), timeout=15) as resp:
            page = json.load(resp)["interests"]
        rows += page
        if len(page) < 100:
            return rows
        offset += 100


def _key(direction, stops):
    return (direction, tuple((s["lat"], s["lon"]) for s in stops))


def plan_company(people, company, office, org, args):
    """All of one company's routes across the week, merged by identical stops; plus the per-day report lines."""
    merged, report = {}, []
    for direction in DIRECTIONS:
        for day in naming.DAYS:
            day_routes = planner.plan_day(people, company, office, direction, day, args.min_riders, args.max_stops)
            if not day_routes:
                continue
            riders = [r for route in day_routes for r in route["riders"]]
            attention = [r for r in riders if r["kind"] in ("DOORSTEP", "WALK_TO_ROUTE")]
            report.append(f"  {day:<9} {direction:<7} {len(riders)} riders on {len(day_routes)} routes, longest {max(r['offsets'][-1] for r in day_routes)} min")
            for r in attention:
                why = "doorstep pickup (detour)" if r["kind"] == "DOORSTEP" else f"walks {r['walkM']} m (said {r['walkLimitM']} m) to {r['stop']}"
                report.append(f"      {r['name'] or r['personId']}: {why}")
            for route in day_routes:
                entry = merged.setdefault(_key(direction, route["stops"]), {**route, "direction": direction, "deps": {}, "riders": {}})
                for dep in route["departures"]:
                    entry["deps"].setdefault(dep, set()).add(day)
                    # how many of that day's riders this departure carries, for the published timetable
                    entry["riders"].setdefault(dep, {})[day] = sum(1 for r in route["riders"] if r["departure"] == dep)
    ordered = sorted(merged.values(), key=lambda r: (DIRECTIONS.index(r["direction"]), min(r["deps"])))
    routes = []
    for n, r in enumerate(ordered, 1):
        days = sorted({d for ds in r["deps"].values() for d in ds}, key=naming.DAYS.index)
        names = [s["name"] for s in r["stops"]]
        routes.append({
            "n": n,
            "name": " → ".join(x for i, x in enumerate(names) if i == 0 or x != names[i - 1]),
            "direction": "TO_OFFICE" if r["direction"] == "MORNING" else "TO_HOME",
            "stops": [{"name": s["name"], "lat": s["lat"], "lon": s["lon"]} for s in r["stops"]],
            "stopOffsetsMinutes": r["offsets"],
            "departures": [
                {"time": f"{t // 60:02d}:{t % 60:02d}", "days": sorted(ds, key=naming.DAYS.index), "riders": {d: r["riders"][t][d] for d in sorted(ds, key=naming.DAYS.index)}}
                for t, ds in sorted(r["deps"].items())
            ],
            "serviceDays": days,
            "startDate": args.start,
            "endDate": args.end,
            "fareInr": args.fare,
            "shape": r["shape"],
        })
    return {"org": org, "companyName": company, "routes": routes}, report


def main(argv):
    today = datetime.date.today()
    ap = argparse.ArgumentParser(description="Plan every partner company's shuttle routes from its interest forms.")
    ap.add_argument("city")
    ap.add_argument("interests", nargs="*", help="saved shuttleInterest/list responses (or use --from-api)")
    ap.add_argument("--from-api", action="store_true", help="read the interest forms from the local dashboard API")
    ap.add_argument("--start", default=today.strftime("%Y%m%d"))
    ap.add_argument("--end", default=(today + datetime.timedelta(days=365)).strftime("%Y%m%d"))
    ap.add_argument("--fare", type=float, default=40)
    ap.add_argument("--min-riders", type=int, default=2)
    ap.add_argument("--max-stops", type=int, default=6)
    args = ap.parse_args(argv[1:])
    city = naming.check_city(args.city)
    config = plans.load_city_config(city)
    if args.from_api:
        path = os.path.join(OUT, f"{city}_interests.json")
        os.makedirs(OUT, exist_ok=True)
        with open(path, "w") as f:
            json.dump(fetch_interests(config), f)
        sources = [path]
    elif args.interests:
        sources = args.interests
    else:
        ap.error("give interest files or --from-api")
    people = interest_io.load(sources)
    companies = sorted({p["company"] for p in people})
    out_dir = plans.plans_dir(city)
    os.makedirs(out_dir, exist_ok=True)
    report = [f"{city}: {len(people)} interest forms, {len(companies)} companies (OSRM {planner.osrm.OSRM_URL})"]
    for company in companies:
        rows = [p for p in people if p["company"] == company]
        office = {"lat": geo.median([p["officeLat"] for p in rows]), "lon": geo.median([p["officeLon"] for p in rows])}
        org = org_code(company, config)
        company_plan, lines = plan_company(rows, company, office, org, args)
        plans.check_plan(company_plan)  # the plan must be one build_feed.py accepts
        with open(os.path.join(out_dir, f"{org}.json"), "w") as f:
            json.dump(company_plan, f, indent=1)
            f.write("\n")
        report.append(f"{company} ({org}): {len(rows)} employees, {len(company_plan['routes'])} routes -> plans/{city}/{org}.json")
        report += lines
    text = "\n".join(report) + "\n"
    os.makedirs(OUT, exist_ok=True)
    with open(os.path.join(OUT, f"{city}_plan_report.txt"), "w") as f:
        f.write(text)
    print(text, end="")
    print(f"next: python3 build_feed.py {city} && python3 validate.py {city}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
