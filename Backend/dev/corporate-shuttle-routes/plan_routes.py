#!/usr/bin/env python3
"""Plan every partner company's shuttle routes from its interest forms (the offline planning step).

  python3 plan_routes.py <city> --from-api                    # read the forms from the local dashboard API
  python3 plan_routes.py <city> interests.json [page2.json]   # or from saved shuttleInterest/list responses
        [--start YYYYMMDD] [--end YYYYMMDD] [--fare 40]

For each company and direction (MORNING to the office, EVENING home), planner.plan_direction lays out routes that carry
every rider along the roads, the same stops on every day, and each day's shuttles (see planner.py): a departure carries
the days it runs and its riders per day. Writes plans/<city>/<ORG>.json, one per company, in the format build_feed.py
reads (naming.py: CORP-<ORG>-<n>), and out/<city>_plan_report.txt with each route's stops and shuttles.

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


def org_codes(companies, config):
    """{company: code}; stops when two companies would get the same code (their plans would overwrite each other)."""
    codes = {c: org_code(c, config) for c in companies}
    clashes = {}
    for company, code in codes.items():
        clashes.setdefault(code, []).append(company)
    clashes = {code: names for code, names in clashes.items() if len(names) > 1}
    if clashes:
        lines = "; ".join(f"{code}: {', '.join(sorted(names))}" for code, names in sorted(clashes.items()))
        raise SystemExit(f"error: companies share an org code ({lines}). Give each its own code under \"orgs\" in cities/<city>.json")
    return codes


def _hhmm(t):
    return f"{t // 60:02d}:{t % 60:02d}"


def plan_company(people, company, office, org, args):
    """All of one company's routes: per direction one layout for the whole week, its departures carrying their days."""
    planned = []
    for direction in DIRECTIONS:
        for route in planner.plan_direction(people, company, office, direction):
            deps = {}
            for day, trips in route["trips"].items():
                for t, ids in trips:
                    deps.setdefault(t, {})[day] = len(ids)
            if deps:
                planned.append({**route, "direction": direction, "deps": deps})
    planned.sort(key=lambda r: (DIRECTIONS.index(r["direction"]), min(r["deps"])))
    routes, report = [], []
    for n, r in enumerate(planned, 1):
        names = [s["name"] for s in r["stops"]]
        name = " → ".join(x for i, x in enumerate(names) if i == 0 or x != names[i - 1])
        departures = [{"time": _hhmm(t), "days": [d for d in naming.DAYS if d in ds], "riders": {d: ds[d] for d in naming.DAYS if d in ds}} for t, ds in sorted(r["deps"].items())]
        routes.append({
            "n": n,
            "name": name,
            "direction": "TO_OFFICE" if r["direction"] == "MORNING" else "TO_HOME",
            "stops": [{"name": s["name"], "lat": s["lat"], "lon": s["lon"]} for s in r["stops"]],
            "stopOffsetsMinutes": r["offsets"],
            "departures": departures,
            "serviceDays": [d for d in naming.DAYS if any(d in dep["days"] for dep in departures)],
            "startDate": args.start,
            "endDate": args.end,
            "fareInr": args.fare,
            "shape": r["shape"],
        })
        shuttles = ", ".join(f"{d['time']} ({max(d['riders'].values())} riders{', light' if max(d['riders'].values()) < planner.MIN_LOAD else ''}, {len(d['days'])} days)" for d in departures)
        report.append(f"  {naming.route_id(org, n)} {r['direction']:<7} {name}: {shuttles}")
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
    codes = org_codes(companies, config)  # all checked before any plan is written
    report = [f"{city}: {len(people)} interest forms, {len(companies)} companies (OSRM {planner.osrm.OSRM_URL})"]
    for company in companies:
        rows = [p for p in people if p["company"] == company]
        office = {"lat": geo.median([p["officeLat"] for p in rows]), "lon": geo.median([p["officeLon"] for p in rows])}
        org = codes[company]
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
