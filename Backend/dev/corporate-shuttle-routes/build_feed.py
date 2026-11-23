#!/usr/bin/env python3
"""Build a city's corporate shuttle GTFS feed (corp_<city>) from every company plan in plans/<city>/.

  python3 build_feed.py chennai

Writes, under out/:
  <city>.corp_shuttle.gtfs/        the feed's txt files, flat (for gtfstidy and a local OTP run)
  <city>.corp_shuttle.gtfs.zip     the same files inside a <city>.corp_shuttle.gtfs/ folder: the nandi/assets layout
                                   (the Nandi Dockerfile unzips every asset into one dir, so a flat zip would spill)
  <city>_fares.sql                 the seat fares for the rider DB (frfs_fare_policy, frfs_route_fare_product,
                                   route_stop_fare), as Backend/dev/local-testing-data/shared-route-extra-setup.sql section 3

Fares stay out of the GTFS, as in the shared cab's feed. The output is deterministic: the same plans give byte-identical
files, so a rebuild with no plan change is a no-op.
"""

import csv
import hashlib
import io
import os
import sys
import zipfile

import naming
import plan as plans

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "out")
ZIP_DATE = (1980, 1, 1, 0, 0, 0)  # fixed timestamp inside the zip, so rebuilds are byte-identical


def _csv(header, rows):
    buf = io.StringIO()
    w = csv.writer(buf, lineterminator="\n")
    w.writerow(header)
    w.writerows(rows)
    return buf.getvalue()


def _fmt(x):
    """Coordinates and amounts without float noise (12.9 not 12.900000000000002)."""
    return format(x, ".6f").rstrip("0").rstrip(".") if isinstance(x, float) else str(x)


def build_files(city, city_config, company_plans):
    """The feed's txt files as {name: text}."""
    agency = naming.agency_id(city)
    stops_rows, routes_rows, trips_rows, times_rows, shape_rows, services = [], [], [], [], [], {}
    for company in company_plans:
        stop_ids, route_stops = plans.resolve_stop_ids(company)
        for (lat, lon), (sid, name) in sorted(stop_ids.items(), key=lambda kv: int(kv[1][0].rsplit("S", 1)[1])):
            stops_rows.append([sid, sid, name, _fmt(lat), _fmt(lon), 0])
        for route in company["routes"]:
            rid = route["routeId"]
            routes_rows.append([rid, agency, f"{company['org']} {route['n']}", route["name"], 3])
            headsign = route["stops"][-1]["name"]
            shape_id = rid if route.get("shape") else ""
            for i, (lat, lon) in enumerate(route.get("shape") or [], 1):
                shape_rows.append([shape_id, _fmt(round(lat, 6)), _fmt(round(lon, 6)), i])
            for dep in route["departures"]:
                # one GTFS service per distinct set of days: a departure runs on exactly the days its riders travel
                sid = naming.service_id(dep["days"], route["startDate"], route["endDate"])
                services[sid] = (dep["days"], route["startDate"], route["endDate"])
                tid = naming.trip_id(rid, dep["time"])
                trips_rows.append([rid, sid, tid, 0, headsign, shape_id])
                start = naming.parse_hhmm(dep["time"])
                for seq, (stop, offset) in enumerate(zip(route_stops[rid], route["offsets"]), 1):
                    t = naming.gtfs_time(start + offset)
                    times_rows.append([tid, t, t, stop, seq, 1])
    calendar_rows = []
    for sid in sorted(services):
        days, start_date, end_date = services[sid]
        calendar_rows.append([sid] + [1 if d in days else 0 for d in naming.DAYS] + [start_date, end_date])

    files = {
        "agency.txt": _csv(["agency_id", "agency_name", "agency_url", "agency_timezone", "agency_lang"], [[agency, f"Namma Yatri Corporate Shuttle {city_config['cityName']}", "https://nammayatri.in", city_config["timezone"], "en"]]),
        "stops.txt": _csv(["stop_id", "stop_code", "stop_name", "stop_lat", "stop_lon", "location_type"], stops_rows),
        "routes.txt": _csv(["route_id", "agency_id", "route_short_name", "route_long_name", "route_type"], routes_rows),
        "trips.txt": _csv(["route_id", "service_id", "trip_id", "direction_id", "trip_headsign", "shape_id"], trips_rows),
        "stop_times.txt": _csv(["trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence", "timepoint"], times_rows),
        "calendar.txt": _csv(["service_id"] + naming.DAYS + ["start_date", "end_date"], calendar_rows),
    }
    if shape_rows:
        files["shapes.txt"] = _csv(["shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence"], shape_rows)
    # the feed version is a hash of its content: it changes exactly when the feed does
    version = hashlib.sha256("".join(files[k] for k in sorted(files)).encode()).hexdigest()[:12]
    starts = [r["startDate"] for c in company_plans for r in c["routes"]]
    ends = [r["endDate"] for c in company_plans for r in c["routes"]]
    files["feed_info.txt"] = _csv(
        ["feed_publisher_name", "feed_publisher_url", "feed_lang", "feed_start_date", "feed_end_date", "feed_version", "feed_id"],
        [["Namma Yatri", "https://nammayatri.in", "en", min(starts), max(ends), version, naming.feed_id(city)]],
    )
    return files


def _sql_text(s):
    return "'" + s.replace("'", "''") + "'"


def build_fares_sql(city, city_config, company_plans):
    """Seat fares for every forward stop pair of every route, idempotent, in the fixture's section 3 shape."""
    c = city_config
    m, moc, ibc, tier, vt = (_sql_text(c[k]) for k in ("merchantId", "merchantOperatingCityId", "integratedBppConfigId", "vehicleServiceTierId", "vehicleType"))
    prefix = c["idSeedPrefix"]
    lines = [
        f"-- Corporate shuttle seat fares for feed {naming.feed_id(city)}: generated by Backend/dev/corporate-shuttle-routes/build_feed.py",
        "-- from plans/" + city + "/*.json. Do not edit by hand; change the plan and rebuild. Idempotent: re-running replaces each",
        "-- route's fares. Run against the rider DB of the environment whose ids are in cities/" + city + ".json.",
        "",
    ]
    for company in company_plans:
        _, route_stops = plans.resolve_stop_ids(company)
        for route in company["routes"]:
            rid = route["routeId"]
            fp = _sql_text(naming.pg_md5_uuid(f"{prefix}-fp-{rid}"))
            rfp = _sql_text(naming.pg_md5_uuid(f"{prefix}-rfp-{rid}"))
            seq = route_stops[rid]
            lines += [
                f"-- {rid}: {route['name']} ({len(seq)} stops)",
                f"INSERT INTO atlas_app.frfs_fare_policy (id,type,description,merchant_id,merchant_operating_city_id,created_at,updated_at)",
                f"VALUES ({fp},'MatrixBased',{_sql_text('corporate shuttle seat fare ' + rid)},{m},{moc},now(),now()) ON CONFLICT DO NOTHING;",
                f"INSERT INTO atlas_app.frfs_route_fare_product (fare_policy_id,id,integrated_bpp_config_id,merchant_id,merchant_operating_city_id,route_code,time_bounds,vehicle_service_tier_id,vehicle_type,created_at,updated_at)",
                f"VALUES ({fp},{rfp},{ibc},{m},{moc},{_sql_text(rid)},'Unbounded',{tier},{vt},now(),now()) ON CONFLICT DO NOTHING;",
                f"DELETE FROM atlas_app.route_stop_fare WHERE fare_policy_id = {fp};",
                "INSERT INTO atlas_app.route_stop_fare (amount,currency,end_stop_code,fare_policy_id,integrated_bpp_config_id,merchant_id,merchant_operating_city_id,start_stop_code,category,created_at,updated_at) VALUES",
            ]
            values = []
            for i in range(1, len(seq) + 1):
                for j in range(i + 1, len(seq) + 1):
                    amount = _fmt(float(plans.fare_of(route, i, j)))
                    values.append(f"  ({amount},'INR',{_sql_text(seq[j - 1])},{fp},{ibc},{m},{moc},{_sql_text(seq[i - 1])},'ADULT',now(),now())")
            lines.append(",\n".join(values) + ";")
            lines.append("")
    return "\n".join(lines)


def write_zip(path, inner_dir, files):
    with zipfile.ZipFile(path, "w", zipfile.ZIP_DEFLATED) as zf:
        for name in sorted(files):
            info = zipfile.ZipInfo(f"{inner_dir}/{name}", date_time=ZIP_DATE)
            info.compress_type = zipfile.ZIP_DEFLATED
            info.external_attr = 0o644 << 16
            zf.writestr(info, files[name])


def build(city, out_dir=OUT):
    city_config = plans.load_city_config(city)
    company_plans = plans.load_city_plans(city)
    files = build_files(city, city_config, company_plans)
    inner = naming.feed_dir_name(city)
    flat_dir = os.path.join(out_dir, inner)
    os.makedirs(flat_dir, exist_ok=True)
    for old in os.listdir(flat_dir):
        if old.endswith(".txt") and old not in files:
            os.remove(os.path.join(flat_dir, old))
    for name, text in files.items():
        with open(os.path.join(flat_dir, name), "w", newline="") as f:
            f.write(text)
    zip_path = os.path.join(out_dir, inner + ".zip")
    write_zip(zip_path, inner, files)
    sql_path = os.path.join(out_dir, f"{city}_fares.sql")
    with open(sql_path, "w") as f:
        f.write(build_fares_sql(city, city_config, company_plans))
    return {"zip": zip_path, "dir": flat_dir, "sql": sql_path, "plans": company_plans}


def main(argv):
    if len(argv) != 2:
        print(__doc__.strip().splitlines()[2].strip(), file=sys.stderr)
        return 2
    try:
        result = build(argv[1])
    except plans.PlanError as e:
        print(f"error: {e}", file=sys.stderr)
        return 1
    print(f"feed {naming.feed_id(argv[1])}: {result['zip']}")
    for company in result["plans"]:
        _, route_stops = plans.resolve_stop_ids(company)
        for route in company["routes"]:
            print(f"  {route['routeId']:<16} {route['direction']:<9} {len(route_stops[route['routeId']])} stops, departures {', '.join(d['time'] for d in route['departures'])}")
    print(f"fares: {result['sql']}")
    print("next: python3 validate.py " + argv[1])
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
