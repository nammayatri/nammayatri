#!/usr/bin/env python3
"""How long each thing the server does actually takes, one request at a time.

This is the honest half of a capacity answer. It sends requests SEQUENTIALLY --
never two at once -- so it puts no more load on the pilot than one passenger
using the app. What it produces is service time: the CPU-seconds one operation
costs. Capacity follows from that and the core count, by the utilisation law,
and is reported as an estimate rather than dressed up as a measured ceiling.

Measuring the real ceiling needs a concurrency ladder, which is a separate
script (`probe-load.py`) and a separate decision, because it deliberately
saturates the box.

RUNS ON THE VPS. From outside, the edge's own rate limit (240 r/m per IP) and
the link from Algeria would both be measured instead of the server.
"""
import http.client
import json
import statistics
import subprocess
import sys
import time
from datetime import datetime, timedelta, timezone

RIDER = ("127.0.0.1", 8013)
DRIVER = ("127.0.0.1", 8017)
SHIM = ("127.0.0.1", 8030)
OSRM = ("127.0.0.1", 5000)
TILES = ("127.0.0.1", 8035)

RIDER_NUM = "0555000199"
RIDER_MERCHANT = "YATRI"
OTP = "7891"
BOSS_TEST_DRIVER = "98821a0c-86b8-42a9-93f7-8260c1af9232"

PICKUP = (36.7538, 3.0588)
DROP = (36.7050, 3.1750)

REPEATS = 12
SEARCH_REPEATS = 6      # this one writes rows; keep the count small and known


def say(m):
    print(m, flush=True)


def pg(sql, db="atlas_dev"):
    out = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres", "-d", db,
         "-At", "-c", sql],
        capture_output=True, text=True, timeout=90)
    return out.stdout.strip()


def call(hostport, method, path, body=None, token=None, conn=None):
    own = conn is None
    if own:
        conn = http.client.HTTPConnection(*hostport, timeout=40)
    headers = {"content-type": "application/json"}
    if token:
        headers["token"] = token
    data = json.dumps(body).encode() if body is not None else None
    try:
        conn.request(method, path, body=data, headers=headers)
        r = conn.getresponse()
        raw = r.read().decode(errors="replace")
        code = r.status
    except Exception as exc:
        return None, 0, str(exc), None
    finally:
        if own:
            conn.close()
            conn = None
    try:
        return json.loads(raw), code, raw, conn
    except Exception:
        return None, code, raw, conn


def cpu_sample():
    with open("/proc/stat") as fh:
        v = [int(x) for x in fh.readline().split()[1:]]
    return v[3] + v[4], sum(v)


def cpu_busy(a, b):
    d = b[1] - a[1]
    return 100.0 * (1.0 - (b[0] - a[0]) / d) if d > 0 else 0.0


def time_it(label, hostport, method, path, token=None, body=None, body_fn=None,
            repeats=REPEATS):
    """One at a time, on one kept-alive connection, like a phone."""
    conn = http.client.HTTPConnection(*hostport, timeout=40)
    times, codes = [], []
    c0 = cpu_sample()
    t_wall = time.time()
    for i in range(repeats):
        payload = body_fn() if body_fn else body
        data = json.dumps(payload).encode() if payload is not None else None
        headers = {"content-type": "application/json"}
        if token:
            headers["token"] = token
        t0 = time.perf_counter()
        try:
            conn.request(method, path, body=data, headers=headers)
            r = conn.getresponse()
            r.read()
            code = r.status
        except Exception:
            code = 0
            try:
                conn.close()
            except Exception:
                pass
            conn = http.client.HTTPConnection(*hostport, timeout=40)
        times.append((time.perf_counter() - t0) * 1000.0)
        codes.append(code)
    wall = max(time.time() - t_wall, 0.001)
    c1 = cpu_sample()
    try:
        conn.close()
    except Exception:
        pass

    times.sort()
    med = statistics.median(times)
    ok = sum(1 for c in codes if 200 <= c < 300)
    busy = cpu_busy(c0, c1)
    # Whole-box CPU-seconds spent per request, generator included. It is the
    # figure that decides how many of these six cores one request occupies.
    core_s = (busy / 100.0) * 6.0 * wall / repeats
    say(f"  {label:<44} {med:>7.1f} ms   min {times[0]:>6.1f}  max {times[-1]:>7.1f}"
        f"   {ok}/{repeats} ok   {core_s*1000:>6.0f} core-ms/req")
    return {"label": label, "median_ms": med, "core_s": core_s,
            "ok": ok, "n": repeats, "codes": sorted(set(codes))}


say("=" * 84)
say("SERVICE TIME -- how long each operation takes, measured one at a time")
say(f"started {datetime.now(timezone.utc).isoformat(timespec='seconds')}")
say("=" * 84)

cores = int(subprocess.run(["nproc"], capture_output=True, text=True).stdout.strip())
idle0 = cpu_sample()
time.sleep(3)
idle1 = cpu_sample()
IDLE_BUSY = cpu_busy(idle0, idle1)
say(f"box: {cores} cores, {IDLE_BUSY:.0f}% busy with NOBODY using it "
    f"(the services' own background work)")

say("")
say("--- signing in")
a, code, raw, _ = call(RIDER, "POST", "/v2/auth", {
    "mobileNumber": RIDER_NUM, "mobileCountryCode": "+213",
    "merchantId": RIDER_MERCHANT})
if not isinstance(a, dict) or "authId" not in a:
    say(f"  rider auth FAILED {code}: {raw[:200]}")
    sys.exit(1)
v, code, raw, _ = call(RIDER, "POST", f"/v2/auth/{a['authId']}/verify",
                       {"otp": OTP, "deviceToken": "svctime"})
RTOK = v.get("token") if isinstance(v, dict) else None
if not RTOK:
    say(f"  rider verify FAILED {code}: {raw[:200]}")
    sys.exit(1)
say("  rider ok")

cred = pg(f"""SELECT p.unencrypted_mobile_number || '~' || p.mobile_country_code
                     || '~' || m.id || '~' || p.id
                FROM atlas_driver_offer_bpp.person p
                JOIN atlas_driver_offer_bpp.merchant m ON m.id = p.merchant_id
               WHERE p.role='DRIVER' AND p.unencrypted_mobile_number IS NOT NULL
                 AND p.mobile_country_code = '+213'
                 AND p.id <> '{BOSS_TEST_DRIVER}'
               LIMIT 1;""")
DTOK = DID = HOME = None
if cred:
    DNUM, DCC, DMER, DID = cred.split("~")
    HOME = pg("SELECT lat || ',' || lon FROM atlas_driver_offer_bpp.driver_location "
              f"WHERE driver_id='{DID}';")
    a, code, raw, _ = call(DRIVER, "POST", "/ui/auth", {
        "mobileNumber": DNUM, "mobileCountryCode": DCC, "merchantId": DMER})
    if isinstance(a, dict) and "authId" in a:
        v, code, raw, _ = call(DRIVER, "POST", f"/ui/auth/{a['authId']}/verify",
                               {"otp": OTP, "deviceToken": "svctime"})
        DTOK = v.get("token") if isinstance(v, dict) else None
    say(f"  driver ok (id={DID}, position {HOME} -- restored at the end)"
        if DTOK else f"  driver auth failed: {raw[:120]}")

_cursor = [datetime.now(timezone.utc)]


def loc_body():
    _cursor[0] += timedelta(seconds=1)
    return [{"pt": {"lat": PICKUP[0] + 0.002, "lon": PICKUP[1] + 0.002},
             "ts": _cursor[0].isoformat().replace("+00:00", "Z"), "acc": 8.0}]


before = {
    "bytes": int(pg("SELECT pg_database_size('atlas_dev')") or 0),
    "searches": int(pg("SELECT count(*) FROM atlas_app.search_request") or 0),
    "beckn": int(pg("SELECT count(*) FROM atlas_app.beckn_request") or 0),
}

say("")
say("--- reads (nothing is written)")
out = []
out.append(time_it("place search, one keystroke",
                   SHIM, "GET",
                   "/place/autocomplete/json?input=bab&location=36.7538,3.0588"))
out.append(time_it("route between two points (OSRM)",
                   OSRM, "GET",
                   "/route/v1/driving/3.0588,36.7538;3.1731,36.7372?overview=false"))
# Both tile paths, because they cost wildly different amounts and the app is
# currently pointed at the expensive one: the style document tileserver serves
# advertises `/tiles/styles/basic-preview/{z}/{x}/{y}.png`, which is rendered
# to an image on the server for every request. `/data/v3/...pbf` is the same
# map handed over as raw vector data for the phone to draw.
out.append(time_it("map tile, vector (phone draws it)",
                   TILES, "GET", "/data/v3/12/2082/1597.pbf"))
out.append(time_it("map tile, raster (SERVER draws it) <- app uses this",
                   TILES, "GET", "/styles/basic-preview/12/2082/1597.png"))
out.append(time_it("rider profile (authenticated)",
                   RIDER, "GET", "/v2/profile", token=RTOK))

if DTOK:
    say("")
    say("--- writes")
    out.append(time_it("driver position update",
                       DRIVER, "POST", "/ui/driver/location",
                       token=DTOK, body_fn=loc_body))

say("")
say("--- the expensive one: a ride request (prices, routes, and pings drivers)")
search_body = {
    "fareProductType": "ONE_WAY",
    "contents": {
        "origin": {"address": {"area": "Alger Centre", "city": "Alger"},
                   "gps": {"lat": PICKUP[0], "lon": PICKUP[1]}},
        "destination": {"address": {"area": "Hussein Dey", "city": "Alger"},
                        "gps": {"lat": DROP[0], "lon": DROP[1]}}}}
out.append(time_it("ride request (search)", RIDER, "POST", "/v2/rideSearch",
                   token=RTOK, body=search_body, repeats=SEARCH_REPEATS))

# ------------------------------------------------------------ what it cost on disk
pg("VACUUM ANALYZE")
after = {
    "bytes": int(pg("SELECT pg_database_size('atlas_dev')") or 0),
    "searches": int(pg("SELECT count(*) FROM atlas_app.search_request") or 0),
    "beckn": int(pg("SELECT count(*) FROM atlas_app.beckn_request") or 0),
}
say("")
say("=" * 84)
say("WHAT DATA COSTS")
say("=" * 84)
d_b = after["bytes"] - before["bytes"]
d_s = after["searches"] - before["searches"]
say(f"database grew {d_b/1024:.0f} kB across {d_s} ride requests "
    f"(+{after['beckn']-before['beckn']} protocol log rows)")
if d_s > 0:
    say(f"  -> one ride request costs {d_b/d_s/1024:.1f} kB on disk, measured "
        f"end to end (all tables, all indexes, all logs)")

say("")
say("average row width on the real rows (heap only):")
for label, tbl in [("a rider account", "atlas_app.person"),
                   ("a driver account", "atlas_driver_offer_bpp.person"),
                   ("a booking", "atlas_app.booking"),
                   ("a ride", "atlas_app.ride"),
                   ("a protocol log row", "atlas_app.beckn_request")]:
    w = pg(f"SELECT coalesce(round(avg(pg_column_size(t.*))),0) FROM {tbl} t")
    say(f"  {label:<22} {w or '?':>7} bytes")

say("")
say("index overhead, on the tables big enough for the ratio to mean anything:")
say(pg("""SELECT c.relname || '  heap ' || pg_size_pretty(pg_relation_size(c.oid))
                 || '  indexes ' || pg_size_pretty(pg_indexes_size(c.oid))
                 || '  x' || round(
                      pg_indexes_size(c.oid)::numeric
                      / greatest(pg_relation_size(c.oid),1), 2)
            FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
           WHERE c.relkind='r' AND n.nspname IN ('atlas_app','atlas_driver_offer_bpp')
             AND pg_relation_size(c.oid) > 100000
           ORDER BY pg_total_relation_size(c.oid) DESC LIMIT 8"""))

say("")
say("the place index (self-hosted geocoder), for scale:")
say(pg("SELECT count(*) || ' places, ' "
       "|| pg_size_pretty(pg_total_relation_size('geo.place')) FROM geo.place"))

free = subprocess.run(["df", "-B1", "--output=avail", "/"],
                      capture_output=True, text=True).stdout.split()[-1]
say("")
say(f"disk free right now: {int(free)/1e9:.0f} GB")

# ----------------------------------------------------------------- restore state
if DTOK and HOME:
    la, lo = HOME.split(",")
    pg(f"UPDATE atlas_driver_offer_bpp.driver_location SET lat={la}, lon={lo} "
       f"WHERE driver_id='{DID}'")
    say(f"driver position restored to {HOME}")

say("")
say("=" * 84)
say("ESTIMATE (utilisation law: one request occupies core-ms; six cores exist)")
say("=" * 84)
usable = cores * (1.0 - IDLE_BUSY / 100.0)
say(f"idle background work already uses {IDLE_BUSY:.0f}% of the box, "
    f"leaving about {usable:.1f} cores for traffic")
for r in out:
    if r["core_s"] <= 0:
        continue
    say(f"  {r['label']:<44} ~{usable/r['core_s']:>8.0f} /s sustained "
        f"({usable/r['core_s']*3600:,.0f} /hour)")
say("")
say("done " + datetime.now(timezone.utc).isoformat(timespec="seconds"))
