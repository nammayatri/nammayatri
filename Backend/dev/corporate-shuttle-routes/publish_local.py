#!/usr/bin/env python3
"""Publish a city's corporate shuttle feed into the LOCAL stack, the way production publishes one (nandi/docs/RELEASE.md):
the GTFS zip goes through Nandi's GTFS preprocessor and GIMS serves the result, and rider-app reads routes from GIMS.

  python3 publish_local.py <city> [--start-gims] [--skip-db]

  1. builds and validates the feed (build_feed.py, validate.py)
  2. runs Nandi's preprocessor (NANDI_DIR/scripts/gtfs-preprocessor, used as it is) on the zip, into out/gims/processed
  3. writes out/gims/gims-local.dhall: GIMS's own dev config, serving only that preprocessed data, all URLs local
  4. --start-gims: (re)starts the GIMS binary already built in GIMS_DIR (target/release/gtfs-routes-service) on it
  5. unless --skip-db: adds the feed to the local rider DB (integrated_bpp_config rows corp_<city>, a SHUTTLE service tier,
     the city's GIMS/OTP endpoints), its seat fares and its timetable (shuttle_published_trip: the days each trip runs
     and its riders, which GIMS does not keep), then clears rider-app's cached feed configs in Redis

Then restart rider-app (it keeps GIMS routes in memory for up to an hour) and the routes are live: rider-app's FRFS
APIs and the control centre's Published routes read them from GIMS. LOCAL ONLY: never run it against a shared database.

Nothing in NANDI_DIR or GIMS_DIR is written: the preprocessor writes only to out/, GIMS's config is a file in out/.
"""

import argparse
import os
import shutil
import signal
import subprocess
import sys
import time
import urllib.request

import build_feed
import naming
import plan as plans
import validate

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "out")
ROOT = os.path.abspath(os.path.join(HERE, "..", "..", "..", ".."))  # the folder holding nammayatri, nandi, GIMS
NANDI_DIR = os.environ.get("NANDI_DIR", os.path.join(ROOT, "nandi"))
GIMS_DIR = os.environ.get("GIMS_DIR", os.path.join(ROOT, "gtfs-inmemory-server-rust"))
PSQL = os.environ.get("PSQL", shutil.which("psql") or "/opt/homebrew/opt/libpq/bin/psql")
DB = os.environ.get("LOCAL_DB_URL", "postgresql://atlas_superuser@localhost:5434/atlas_dev")
REDIS_CLI = os.environ.get("REDIS_CLI", shutil.which("redis-cli") or "")
REDIS_PORTS = [6379] + list(range(30001, 30007))  # the local stack's redis and redis cluster nodes
# the corporate shuttle demand knobs (Domain.Types.SharedRoute), as the shared-route fixture sets them
SHUTTLE_KNOBS = '{"route":"FIXED","schedule":"TIMETABLED","seats":"RESERVED","booking":"PRE_BOOK","allocation":"PRE_ASSIGNED","boarding":"OTP","cancelCutoffMinutes":30,"noShowConsequence":"FORFEIT_TRIP"}'
LOCAL_CIPHER_KEY = "TXlTZWNyZXRLZXkxMjM0NQo="  # local dev only; the DIRECT provider needs one


def step(msg):
    print(f"== {msg}", flush=True)


def build_and_validate(city, config):
    result = build_feed.build(city)
    with open(result["sql"]) as f:
        fares = f.read()
    errors = {k: v for k, v in validate.check(city, validate.load_zip(result["zip"]), fares, config).items() if v}
    if errors:
        sys.exit(f"feed does not validate: {errors}")
    return result


def preprocess(zip_path):
    assets, processed = os.path.join(OUT, "gims", "assets"), os.path.join(OUT, "gims", "processed")
    for d in (assets, processed):
        shutil.rmtree(d, ignore_errors=True)
        os.makedirs(d)
    shutil.copy(zip_path, assets)
    script = os.path.join(NANDI_DIR, "scripts", "gtfs-preprocessor", "gtfs_preprocessor.py")
    if not os.path.exists(script):
        sys.exit(f"Nandi's preprocessor not found at {script} (set NANDI_DIR)")
    env = {**os.environ, "PYTHONDONTWRITEBYTECODE": "1"}  # leave no __pycache__ in the nandi checkout
    subprocess.run([sys.executable, "-B", script, "--assets-dir", assets, "--output-dir", processed], check=True, env=env, cwd=os.path.dirname(script))
    return processed


def gims_config(processed, port):
    src = os.path.join(GIMS_DIR, "dhall-configs", "dev", "gtfs_in_memory_server_rust.dhall")
    text = open(src).read()
    text = text.replace("../secrets/gtfs_in_memory_server_rust.example.dhall", os.path.join(GIMS_DIR, "dhall-configs", "secrets", "gtfs_in_memory_server_rust.example.dhall"))
    for old, new in (("port = 8000,", f"port = {port},"), ("use_preprocessed_data = False,", "use_preprocessed_data = True,"), ('preprocessed_data_dir = "./assets",', f'preprocessed_data_dir = "{processed}",')):
        if old not in text:
            sys.exit(f"GIMS dev config changed shape: '{old}' not found in {src}")
        text = text.replace(old, new)
    text = text.replace("https://api.sandbox.moving.tech/nandi", "http://localhost:8089")  # keep everything local
    path = os.path.join(OUT, "gims", "gims-local.dhall")
    with open(path, "w") as f:
        f.write(f"-- LOCAL ONLY, written by publish_local.py from {src}: serves the preprocessed corporate feed\n" + text)
    return path


def start_gims(config_path, port, feed):
    for pid in subprocess.run(["lsof", "-ti", f"tcp:{port}", "-sTCP:LISTEN"], capture_output=True, text=True).stdout.split():
        os.kill(int(pid), signal.SIGTERM)
    time.sleep(1)
    binary = os.path.join(GIMS_DIR, "target", "release", "gtfs-routes-service")
    if not os.path.exists(binary):
        sys.exit(f"GIMS is not built: {binary} (cargo build --release in {GIMS_DIR})")
    log = open(os.path.join(OUT, "gims", "gims.log"), "w")
    subprocess.Popen([binary], env={**os.environ, "DHALL_CONFIG": config_path}, cwd=os.path.join(OUT, "gims"), stdout=log, stderr=log, start_new_session=True)
    for _ in range(60):
        try:
            with urllib.request.urlopen(f"http://localhost:{port}/routes/{feed}", timeout=2) as resp:
                if resp.status == 200:
                    return
        except OSError:
            time.sleep(1)
    sys.exit(f"GIMS did not come up on :{port}; see out/gims/gims.log")


def sql_text(s):
    return "'" + s.replace("'", "''") + "'"


def feed_config_sql(city, c):
    feed = naming.feed_id(city)
    m, moc = sql_text(c["merchantId"]), sql_text(c["merchantOperatingCityId"])
    direct = '{"tag":"DIRECT","contents":{"cipherKey":"%s","redisPrefix":"%s","sharedKnobs":%s}}' % (LOCAL_CIPHER_KEY, feed, SHUTTLE_KNOBS)
    gims = '{"baseUrl":"%s","queryType":"MULTI_SEARCH","numItineraries":20,"weightedSortCfg":{"duration":0.47,"transfers":0.05,"arrivalTime":0.48}}' % c["gimsUrl"]
    otp = gims.replace(c["gimsUrl"], "http://localhost:8089/otp/gtfs/v1/")
    rows = []
    for ibc_id, platform in ((c["integratedBppConfigId"], "MULTIMODAL"), (c["applicationIntegratedBppConfigId"], "APPLICATION")):
        rows.append(
            f"INSERT INTO atlas_app.integrated_bpp_config (id, agency_key, domain, feed_key, merchant_id, merchant_operating_city_id, platform_type, config_json, vehicle_category)\n"
            f"VALUES ({sql_text(ibc_id)}, {sql_text(feed)}, 'FRFS', {sql_text(feed)}, {m}, {moc}, '{platform}', {sql_text(direct)}, 'BUS')\n"
            f"ON CONFLICT (id) DO UPDATE SET config_json = EXCLUDED.config_json, feed_key = EXCLUDED.feed_key, agency_key = EXCLUDED.agency_key, updated_at = now();"
        )
    rows.append(
        "INSERT INTO atlas_app.frfs_vehicle_service_tier (type, description, id, integrated_bpp_config_id, long_name, merchant_id, merchant_operating_city_id, provider_code, short_name, is_air_conditioned, pay_on_board)\n"
        f"VALUES ('SHUTTLE', 'Corporate shuttle', {sql_text(c['vehicleServiceTierId'])}, {sql_text(c['integratedBppConfigId'])}, 'Corporate Shuttle', {m}, {moc}, 'SHUTTLE', 'Shuttle', true, false) ON CONFLICT DO NOTHING;"
    )
    for service, body in (("MultiModalStaticData_OTPTransit", gims), ("MultiModal_OTPTransit", otp)):
        rows.append(f"DELETE FROM atlas_app.merchant_service_config WHERE merchant_operating_city_id = {moc} AND service_name = '{service}';")
        rows.append(f"INSERT INTO atlas_app.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json) VALUES ({m}, {moc}, '{service}', {sql_text(body)});")
    header = f"-- LOCAL ONLY, written by publish_local.py: feed {feed} in the local rider DB (rider-app reads its routes from GIMS at {c['gimsUrl']})\n"
    return header + "\n".join(rows) + "\n"


def timetable_sql(city, c, company_plans):
    """shuttle_published_trip: the days each published trip runs and its riders that day (GIMS does not keep the trips'
    calendar). Replaces the feed's rows, so it always matches the feed just published."""
    feed = naming.feed_id(city)
    m, moc = sql_text(c["merchantId"]), sql_text(c["merchantOperatingCityId"])
    rows = []
    for company in company_plans:
        for route in company["routes"]:
            for dep in route["departures"]:
                trip = naming.trip_id(route["routeId"], dep["time"])
                for day in dep["days"]:
                    rows.append(f"  ({sql_text(trip)}, '{day}', {sql_text(feed)}, {sql_text(route['routeId'])}, {sql_text(dep['time'])}, {int(dep['riders'].get(day, 0))}, {m}, {moc})")
    sql = f"-- LOCAL ONLY, written by publish_local.py: the published timetable of {feed}\n"
    sql += f"DELETE FROM atlas_app.shuttle_published_trip WHERE feed_key = {sql_text(feed)};\n"
    if rows:
        sql += "INSERT INTO atlas_app.shuttle_published_trip (trip_id, day, feed_key, route_code, departure, riders, merchant_id, merchant_operating_city_id) VALUES\n"
        sql += ",\n".join(rows) + ";\n"
    return sql


def psql_file(path):
    subprocess.run([PSQL, DB, "-v", "ON_ERROR_STOP=1", "-q", "-f", path], check=True)


def clear_feed_caches():
    if not REDIS_CLI:
        print("   redis-cli not found: restart rider-app to drop cached feed configs")
        return
    for port in REDIS_PORTS:
        keys = []
        for pattern in ("*CachedQueries:IntegratedBPPConfig*", "*ConfigPilot:IntegratedBPPConfig*", "*MerchantServiceConfig*"):
            keys += subprocess.run([REDIS_CLI, "-p", str(port), "--scan", "--pattern", pattern], capture_output=True, text=True).stdout.split()
        for k in keys:
            subprocess.run([REDIS_CLI, "-p", str(port), "del", k], capture_output=True)


def main(argv):
    ap = argparse.ArgumentParser(description="Publish a city's corporate shuttle feed into the local stack.")
    ap.add_argument("city")
    ap.add_argument("--start-gims", action="store_true", help="(re)start the local GIMS on the published feed")
    ap.add_argument("--skip-db", action="store_true", help="do not touch the local rider DB")
    args = ap.parse_args(argv[1:])
    city = naming.check_city(args.city)
    config = plans.load_city_config(city)
    feed = naming.feed_id(city)
    port = int(config["gimsUrl"].rsplit(":", 1)[1].split("/")[0])

    step(f"build and validate {feed}")
    result = build_and_validate(city, config)
    step("preprocess with Nandi's GTFS preprocessor")
    processed = preprocess(result["zip"])
    step("GIMS config")
    gims_conf = gims_config(processed, port)
    print(f"   {gims_conf}")
    if args.start_gims:
        step(f"start GIMS on :{port}")
        start_gims(gims_conf, port, feed)
        with urllib.request.urlopen(f"http://localhost:{port}/routes/{feed}") as resp:
            print(f"   GIMS serves {feed}: {len(__import__('json').load(resp))} routes")
    else:
        print(f"   start it with: DHALL_CONFIG={gims_conf} {os.path.join(GIMS_DIR, 'target', 'release', 'gtfs-routes-service')}")
    if not args.skip_db:
        step("feed config and fares in the local rider DB")
        sql_path = os.path.join(OUT, f"{city}_feed_config.sql")
        with open(sql_path, "w") as f:
            f.write(feed_config_sql(city, config))
        psql_file(sql_path)
        psql_file(result["sql"])
        timetable_path = os.path.join(OUT, f"{city}_timetable.sql")
        with open(timetable_path, "w") as f:
            f.write(timetable_sql(city, config, result["plans"]))
        psql_file(timetable_path)
        clear_feed_caches()
    print(f"published {feed}. Restart rider-app so it reloads routes from GIMS.")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
