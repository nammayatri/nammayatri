#!/usr/bin/env python3
"""Validate a built corporate shuttle feed: python3 validate.py <city> [--selftest]

Reads out/<city>.corp_shuttle.gtfs.zip and out/<city>_fares.sql (run build_feed.py first) and checks, one function per rule:

  ids          every id follows the convention (naming.py); feed_id and agency_id are corp_<city>; stop_code = stop_id;
               a trip's HHMM is its first departure
  unique       no repeated primary key in any file
  references   trips -> routes, calendar and shapes; stop_times -> trips and stops; every route has a trip, every stop is used
  trips        each trip has >= 2 stops, stop_sequence runs 1..n, times strictly increase; route_type is 3 (bus)
  coordinates  every stop is inside lat/lon range
  companies    a route only uses its own company's stops (CORP-ACME-* uses CORP_ACME_S*)
  fares        out/<city>_fares.sql prices every forward stop pair of every route, once, above zero
  gtfstidy     the flat feed parses in gtfstidy with no errors (inside the nix shell, where gtfstidy is on PATH)

--selftest breaks each rule on purpose and checks that it fires.
Exits non-zero when any rule fails.
"""

import csv
import io
import os
import re
import shutil
import subprocess
import sys
import tempfile
import zipfile

import naming
import plan as plans

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "out")
FARE_ROW_RE = re.compile(r"^\s*\(([0-9.]+),'INR','([^']*)','([^']*)','[^']*','[^']*','[^']*','([^']*)','ADULT',now\(\),now\(\)\)[,;]\s*$")


def read_tables(files):
    return {name[:-4]: list(csv.DictReader(io.StringIO(text))) for name, text in files.items() if name.endswith(".txt")}


def load_zip(path):
    with zipfile.ZipFile(path) as zf:
        return {os.path.basename(n): zf.read(n).decode() for n in zf.namelist() if n.endswith(".txt")}


def rule_ids(city, t):
    errors = []
    fid = naming.feed_id(city)
    if [r.get("feed_id") for r in t.get("feed_info", [])] != [fid]:
        errors.append(f"feed_info must have exactly one row with feed_id {fid}")
    for a in t.get("agency", []):
        if a["agency_id"] != naming.agency_id(city):
            errors.append(f"agency_id {a['agency_id']} must be {naming.agency_id(city)}")
    for r in t.get("routes", []):
        if not naming.ROUTE_RE.match(r["route_id"]):
            errors.append(f"route_id {r['route_id']} is not CORP-<ORG>-<n>")
        if r["agency_id"] != naming.agency_id(city):
            errors.append(f"route {r['route_id']} has agency {r['agency_id']}")
    for s in t.get("stops", []):
        if not naming.STOP_RE.match(s["stop_id"]):
            errors.append(f"stop_id {s['stop_id']} is not CORP_<ORG>_S<n>")
        if s.get("stop_code") != s["stop_id"]:
            errors.append(f"stop {s['stop_id']} has stop_code {s.get('stop_code')} (must equal stop_id)")
    first_departure = {}
    for st in t.get("stop_times", []):
        if st["stop_sequence"] == "1":
            first_departure[st["trip_id"]] = st["departure_time"]
    for tr in t.get("trips", []):
        m = naming.TRIP_RE.match(tr["trip_id"])
        if not m or m.group(1) != tr["route_id"]:
            errors.append(f"trip_id {tr['trip_id']} is not <route>-<HHMM> of route {tr['route_id']}")
        elif tr["trip_id"] in first_departure and first_departure[tr["trip_id"]][:5].replace(":", "") != m.group(2):
            errors.append(f"trip {tr['trip_id']} first departs at {first_departure[tr['trip_id']]}")
    return errors


def rule_unique(city, t):
    keys = {"stops": ["stop_id"], "routes": ["route_id"], "trips": ["trip_id"], "calendar": ["service_id"], "agency": ["agency_id"], "stop_times": ["trip_id", "stop_sequence"]}
    errors = []
    for table, cols in keys.items():
        seen = set()
        for row in t.get(table, []):
            k = tuple(row[c] for c in cols)
            if k in seen:
                errors.append(f"{table}: {'/'.join(k)} appears twice")
            seen.add(k)
    return errors


def rule_references(city, t):
    errors = []
    routes = {r["route_id"] for r in t.get("routes", [])}
    services = {c["service_id"] for c in t.get("calendar", [])}
    trips = {tr["trip_id"] for tr in t.get("trips", [])}
    stops = {s["stop_id"] for s in t.get("stops", [])}
    for tr in t.get("trips", []):
        if tr["route_id"] not in routes:
            errors.append(f"trip {tr['trip_id']} -> unknown route {tr['route_id']}")
        if tr["service_id"] not in services:
            errors.append(f"trip {tr['trip_id']} -> unknown service {tr['service_id']}")
    for st in t.get("stop_times", []):
        if st["trip_id"] not in trips:
            errors.append(f"stop_time -> unknown trip {st['trip_id']}")
        if st["stop_id"] not in stops:
            errors.append(f"stop_time of {st['trip_id']} -> unknown stop {st['stop_id']}")
    shapes = {sh["shape_id"] for sh in t.get("shapes", [])}
    for tr in t.get("trips", []):
        if tr.get("shape_id") and tr["shape_id"] not in shapes:
            errors.append(f"trip {tr['trip_id']} -> unknown shape {tr['shape_id']}")
    used_routes = {tr["route_id"] for tr in t.get("trips", [])}
    used_stops = {st["stop_id"] for st in t.get("stop_times", [])}
    errors += [f"route {r} has no trip" for r in sorted(routes - used_routes)]
    errors += [f"stop {s} is on no trip" for s in sorted(stops - used_stops)]
    return errors


def _minutes(hms):
    h, m, s = (int(x) for x in hms.split(":"))
    return h * 60 + m + s / 60


def rule_trips(city, t):
    errors = []
    for r in t.get("routes", []):
        if r["route_type"] != "3":
            errors.append(f"route {r['route_id']} has route_type {r['route_type']} (must be 3, bus)")
    by_trip = {}
    for st in t.get("stop_times", []):
        by_trip.setdefault(st["trip_id"], []).append(st)
    for tid, rows in sorted(by_trip.items()):
        rows.sort(key=lambda r: int(r["stop_sequence"]))
        if len(rows) < 2:
            errors.append(f"trip {tid} has fewer than 2 stops")
        if [int(r["stop_sequence"]) for r in rows] != list(range(1, len(rows) + 1)):
            errors.append(f"trip {tid} stop_sequence is not 1..{len(rows)}")
        times = [_minutes(r["departure_time"]) for r in rows]
        if any(b <= a for a, b in zip(times, times[1:])):
            errors.append(f"trip {tid} times do not strictly increase")
    return errors


def rule_coordinates(city, t):
    errors = []
    for s in t.get("stops", []):
        try:
            lat, lon = float(s["stop_lat"]), float(s["stop_lon"])
        except ValueError:
            errors.append(f"stop {s['stop_id']} has non-numeric coordinates")
            continue
        if not (-90 <= lat <= 90 and -180 <= lon <= 180):
            errors.append(f"stop {s['stop_id']} coordinates out of range ({lat}, {lon})")
    return errors


def rule_companies(city, t):
    errors = []
    trip_route = {tr["trip_id"]: tr["route_id"] for tr in t.get("trips", [])}
    for st in t.get("stop_times", []):
        rm = naming.ROUTE_RE.match(trip_route.get(st["trip_id"], ""))
        sm = naming.STOP_RE.match(st["stop_id"])
        if rm and sm and rm.group(1) != sm.group(1):
            errors.append(f"route {rm.group(0)} uses another company's stop {st['stop_id']}")
    return sorted(set(errors))


def rule_fares(city, t, fares_sql, city_config):
    """Every forward stop pair of every route has exactly one positive fare row under that route's fare policy."""
    errors = []
    rows = {}
    for line in fares_sql.splitlines():
        m = FARE_ROW_RE.match(line)
        if m:
            amount, end, fp, start = float(m.group(1)), m.group(2), m.group(3), m.group(4)
            rows.setdefault(fp, []).append((start, end, amount))
    order = {}
    first_trip = {}
    for tr in t.get("trips", []):
        first_trip.setdefault(tr["route_id"], tr["trip_id"])
    for st in t.get("stop_times", []):
        order.setdefault(st["trip_id"], []).append((int(st["stop_sequence"]), st["stop_id"]))
    for rid, tid in sorted(first_trip.items()):
        seq = [s for _, s in sorted(order.get(tid, []))]
        expected = {(seq[i], seq[j]) for i in range(len(seq)) for j in range(i + 1, len(seq))}
        fp = naming.pg_md5_uuid(f"{city_config['idSeedPrefix']}-fp-{rid}")
        got = rows.get(fp, [])
        pairs = [(a, b) for a, b, _ in got]
        for a, b in sorted(expected - set(pairs)):
            errors.append(f"{rid}: no fare for {a} -> {b}")
        for a, b in sorted(set(pairs) - expected):
            errors.append(f"{rid}: fare for {a} -> {b}, which is not a forward pair of the route")
        if len(pairs) != len(set(pairs)):
            errors.append(f"{rid}: a stop pair is priced twice")
        errors += [f"{rid}: fare {a} -> {b} is {amt}" for a, b, amt in got if amt <= 0]
    return errors


def rule_gtfstidy(flat_dir):
    """Returns (errors, note). Skipped (with a note) outside the nix shell, where gtfstidy is not on PATH."""
    exe = shutil.which("gtfstidy")
    if not exe:
        return [], "gtfstidy not on PATH: skipped (run inside the nammayatri nix shell)"
    with tempfile.TemporaryDirectory() as tmp:
        proc = subprocess.run([exe, "-o", os.path.join(tmp, "tidy"), flat_dir], capture_output=True, text=True)
    if proc.returncode != 0:
        return [f"gtfstidy: {line}" for line in (proc.stderr or proc.stdout).strip().splitlines()[-10:]], None
    return [], "gtfstidy: parsed with no errors"


RULES = [("ids", rule_ids), ("unique", rule_unique), ("references", rule_references), ("trips", rule_trips), ("coordinates", rule_coordinates), ("companies", rule_companies)]


def check(city, files, fares_sql, city_config):
    """{rule: [errors]} for the feed's files (gtfstidy excluded)."""
    t = read_tables(files)
    result = {name: fn(city, t) for name, fn in RULES}
    result["fares"] = rule_fares(city, t, fares_sql, city_config)
    return result


def report(result, gtfstidy_result=None):
    ok = True
    for name, errors in result.items():
        print(f"  {'ok  ' if not errors else 'FAIL'} {name}")
        for e in errors[:20]:
            print(f"       {e}")
        if len(errors) > 20:
            print(f"       ... {len(errors) - 20} more")
        ok = ok and not errors
    if gtfstidy_result is not None:
        errors, note = gtfstidy_result
        print(f"  {'ok  ' if not errors else 'FAIL'} gtfstidy" + (f" ({note})" if note else ""))
        for e in errors:
            print(f"       {e}")
        ok = ok and not errors
    return ok


def selftest():
    """Break each rule on purpose and check that it fires; the unbroken feed must pass."""
    import build_feed

    city = "selftest"
    config = {"merchantId": "m", "merchantOperatingCityId": "c", "integratedBppConfigId": "i", "vehicleServiceTierId": "t", "vehicleType": "BUS", "timezone": "Asia/Kolkata", "cityName": "Selftest", "idSeedPrefix": "selftest"}
    company = plans.check_plan(
        {
            "org": "ACME",
            "routes": [
                {
                    "n": 1,
                    "name": "test",
                    "stops": [{"name": f"S{i}", "lat": 12.9 + i / 100, "lon": 80.2} for i in range(1, 4)],
                    "minutesBetweenStops": 10,
                    "departures": ["08:30", "18:00"],
                    "serviceDays": ["monday"],
                    "startDate": "20270101",
                    "endDate": "20271231",
                    "fareInr": 40,
                }
            ],
        }
    )
    files = build_feed.build_files(city, config, [company])
    sql = build_feed.build_fares_sql(city, config, [company])
    failures = []
    base = check(city, files, sql, config)
    if any(base.values()):
        failures.append(f"the unbroken feed fails: {base}")

    def breaks(rule, name, old, new, sql_override=None):
        broken = dict(files)
        if name:
            assert old in broken[name], (rule, old)
            broken[name] = broken[name].replace(old, new, 1)
        result = check(city, broken, sql if sql_override is None else sql_override, config)
        if not result[rule]:
            failures.append(f"rule {rule} did not fire for: {old!r} -> {new!r}")

    breaks("ids", "routes.txt", "CORP-ACME-1,", "ACME-1,")
    breaks("ids", "stops.txt", "CORP_ACME_S1,CORP_ACME_S1", "CORP_ACME_S1,X1")
    breaks("ids", "feed_info.txt", "corp_selftest", "corp_other")
    breaks("unique", "stops.txt", "CORP_ACME_S2,CORP_ACME_S2", "CORP_ACME_S1,CORP_ACME_S1")
    breaks("references", "trips.txt", "MON_20270101_20271231,CORP-ACME-1-0830", "NOPE,CORP-ACME-1-0830")
    breaks("trips", "stop_times.txt", "CORP-ACME-1-0830,08:40:00,08:40:00", "CORP-ACME-1-0830,08:20:00,08:20:00")
    breaks("trips", "routes.txt", ",test,3", ",test,2")
    breaks("coordinates", "stops.txt", ",12.91,80.2,", ",99.91,80.2,")
    breaks("companies", "stop_times.txt", "CORP-ACME-1-0830,08:40:00,08:40:00,CORP_ACME_S2", "CORP-ACME-1-0830,08:40:00,08:40:00,CORP_OTHER_S2")
    dropped = "\n".join(line for line in sql.splitlines() if "'CORP_ACME_S3'" not in line or "'CORP_ACME_S1'" not in line)
    breaks("fares", None, None, None, sql_override=dropped)
    breaks("fares", None, None, None, sql_override=sql.replace("(40,'INR'", "(0,'INR'", 1))
    if failures:
        print("selftest FAILED:")
        for f in failures:
            print("  " + f)
        return False
    print("selftest ok: every rule fires on a broken feed, and the unbroken feed passes")
    return True


def main(argv):
    if "--selftest" in argv:
        return 0 if selftest() else 1
    if len(argv) != 2:
        print("usage: python3 validate.py <city> | --selftest", file=sys.stderr)
        return 2
    city = naming.check_city(argv[1])
    inner = naming.feed_dir_name(city)
    zip_path, sql_path = os.path.join(OUT, inner + ".zip"), os.path.join(OUT, f"{city}_fares.sql")
    for p in (zip_path, sql_path):
        if not os.path.exists(p):
            print(f"error: {p} not found; run python3 build_feed.py {city} first", file=sys.stderr)
            return 1
    city_config = plans.load_city_config(city)
    with open(sql_path) as f:
        fares_sql = f.read()
    print(f"validating {zip_path}")
    ok = report(check(city, load_zip(zip_path), fares_sql, city_config), rule_gtfstidy(os.path.join(OUT, inner)))
    print("all rules pass" if ok else "validation FAILED")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
