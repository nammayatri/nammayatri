#!/usr/bin/env python3
"""What one request of each kind costs the server, in CPU.

This is the input to every capacity claim about this pilot. Latency alone
cannot answer "how many at once": a request that takes 300ms but spends it
waiting costs almost nothing, while one that takes 300ms of CPU occupies a
whole core. Capacity follows from the CPU, so the CPU is what is measured.

Method, and why each part of it is there:

  * every endpoint is WARMED first and the warm-up is thrown away. The first
    measurement of the raster tile endpoint came out at 800 core-ms/request
    and implied the map could serve six people. It was one 3-second style
    load, averaged into eleven fast requests. Warm, it is 23ms.
  * the same tile is never asked for twice. Repeating one URL measures a
    cache; a phone panning a map asks for new squares every time.
  * the box's idle CPU is measured immediately before each endpoint and
    SUBTRACTED. Roughly 15% of this machine is busy with background work
    before any traffic arrives, and charging that to the request under test
    would understate capacity by that much.
  * the generator's own CPU is measured from /proc/self/stat and subtracted
    too. It shares the six cores with the services it is testing.
  * requests are sent ONE AT A TIME. This script never puts the pilot under
    load; it measures the cost of a single operation and does the arithmetic.

Status codes are printed. An earlier version reported a throughput for the
driver-position write that was really the cost of a 401, because the token had
been invalidated underneath it by the keepalive job and nothing said so.
"""
import http.client
import json
import statistics
import subprocess
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

CORES = 6


def say(m):
    print(m, flush=True)


def pg(sql, db="atlas_dev"):
    out = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres", "-d", db,
         "-At", "-c", sql], capture_output=True, text=True, timeout=120)
    return out.stdout.strip()


def cpu():
    with open("/proc/stat") as fh:
        v = [int(x) for x in fh.readline().split()[1:]]
    return v[3] + v[4], sum(v)


def busy(a, b):
    d = b[1] - a[1]
    return (1.0 - (b[0] - a[0]) / d) if d > 0 else 0.0


def self_cpu():
    with open("/proc/self/stat") as fh:
        f = fh.readline().split()
    return (int(f[13]) + int(f[14])) / 100.0


def one(conn, hostport, method, path, payload, token):
    headers = {"content-type": "application/json"}
    if token:
        headers["token"] = token
    data = json.dumps(payload).encode() if payload is not None else None
    t0 = time.perf_counter()
    try:
        conn.request(method, path, body=data, headers=headers)
        r = conn.getresponse()
        r.read()
        code = r.status
    except Exception:
        try:
            conn.close()
        except Exception:
            pass
        conn = http.client.HTTPConnection(*hostport, timeout=60)
        code = 0
    return conn, (time.perf_counter() - t0) * 1000.0, code


RESULTS = []


def measure(label, hostport, method, path_fn, n, warm=8, payload_fn=None, token=None):
    """path_fn(i) so no two requests are identical where that matters."""
    conn = http.client.HTTPConnection(*hostport, timeout=60)
    for i in range(warm):
        conn, _, _ = one(conn, hostport, method, path_fn(i),
                         payload_fn(i) if payload_fn else None, token)

    # Idle baseline, taken now rather than once at the top: background work on
    # this box is bursty, and a baseline from two minutes ago is a guess.
    i0 = cpu()
    time.sleep(2.0)
    i1 = cpu()
    idle_frac = busy(i0, i1)

    times, codes = [], []
    c0, s0, w0 = cpu(), self_cpu(), time.time()
    for i in range(n):
        conn, ms, code = one(conn, hostport, method, path_fn(warm + i),
                             payload_fn(warm + i) if payload_fn else None, token)
        times.append(ms)
        codes.append(code)
    wall = max(time.time() - w0, 1e-6)
    c1, s1 = cpu(), self_cpu()
    try:
        conn.close()
    except Exception:
        pass

    total_core_s = busy(c0, c1) * CORES * wall
    idle_core_s = idle_frac * CORES * wall
    gen_core_s = s1 - s0
    net = max(total_core_s - idle_core_s - gen_core_s, 0.0)
    per_req = net / n

    times.sort()
    ok = sum(1 for c in codes if 200 <= c < 300)
    RESULTS.append({"label": label, "per_req": per_req, "median": statistics.median(times),
                    "p95": times[int(0.95 * (len(times) - 1))], "ok": ok, "n": n,
                    "codes": sorted(set(codes))})
    say(f"  {label:<42} {statistics.median(times):>7.1f} ms  p95 {times[int(0.95*(len(times)-1))]:>7.1f}"
        f"   {per_req*1000:>7.1f} core-ms   {ok}/{n} ok {sorted(set(codes))}")


say("=" * 96)
say("COST PER REQUEST -- the input to every capacity number")
say(f"{datetime.now(timezone.utc).isoformat(timespec='seconds')}   {CORES} cores")
say("=" * 96)

b0 = cpu()
time.sleep(4)
b1 = cpu()
IDLE = busy(b0, b1)
say(f"background load with nobody using the app: {IDLE*100:.1f}% "
    f"({IDLE*CORES:.2f} of {CORES} cores)")

# ------------------------------------------------------------------- sign in
code_body = {"mobileNumber": RIDER_NUM, "mobileCountryCode": "+213",
             "merchantId": RIDER_MERCHANT}
conn = http.client.HTTPConnection(*RIDER, timeout=40)
conn, _, _ = one(conn, RIDER, "POST", "/v2/auth", code_body, None)
conn.close()


def http_json(hostport, method, path, payload=None, token=None):
    c = http.client.HTTPConnection(*hostport, timeout=40)
    h = {"content-type": "application/json"}
    if token:
        h["token"] = token
    c.request(method, path, body=json.dumps(payload).encode() if payload is not None else None,
              headers=h)
    r = c.getresponse()
    raw = r.read().decode(errors="replace")
    c.close()
    return r.status, raw


st, raw = http_json(RIDER, "POST", "/v2/auth", code_body)
authId = json.loads(raw)["authId"]
st, raw = http_json(RIDER, "POST", f"/v2/auth/{authId}/verify",
                    {"otp": OTP, "deviceToken": "costprobe"})
RTOK = json.loads(raw)["token"]

cred = pg(f"""SELECT p.unencrypted_mobile_number || '~' || p.mobile_country_code
                     || '~' || m.id || '~' || p.id
                FROM atlas_driver_offer_bpp.person p
                JOIN atlas_driver_offer_bpp.merchant m ON m.id = p.merchant_id
               WHERE p.role='DRIVER' AND p.unencrypted_mobile_number IS NOT NULL
                 AND p.mobile_country_code='+213' AND p.id <> '{BOSS_TEST_DRIVER}'
               LIMIT 1;""")
DNUM, DCC, DMER, DID = cred.split("~")
HOME = pg(f"SELECT lat || ',' || lon FROM atlas_driver_offer_bpp.driver_location "
          f"WHERE driver_id='{DID}';")
st, raw = http_json(DRIVER, "POST", "/ui/auth",
                    {"mobileNumber": DNUM, "mobileCountryCode": DCC, "merchantId": DMER})
authId = json.loads(raw)["authId"]
st, raw = http_json(DRIVER, "POST", f"/ui/auth/{authId}/verify",
                    {"otp": OTP, "deviceToken": "costprobe"})
DTOK = json.loads(raw)["token"]
say(f"signed in: rider ok, driver {DID} ok (position {HOME}, restored at the end)")
say("")
say(f"  {'operation':<42} {'median':>10}  {'p95':>11}   {'cpu cost':>13}   result")

# --------------------------------------------------------------- measurements
WORDS = ["bab", "alger", "hussein", "did", "oran", "bir", "cheraga", "kouba",
         "rouiba", "birkhadem", "dely", "hydra"]
measure("place search (one keystroke typed)", SHIM, "GET",
        lambda i: f"/place/autocomplete/json?input={WORDS[i % len(WORDS)]}"
                  f"&location=36.7538,3.0588", n=60)

measure("route for a fare estimate (OSRM)", OSRM, "GET",
        lambda i: f"/route/v1/driving/3.0588,36.7538;"
                  f"{3.1731 + (i % 20) * 0.001:.4f},36.7372?overview=false", n=200)

measure("map tile, vector (phone draws it)", TILES, "GET",
        lambda i: f"/data/v3/12/{2078 + (i % 8)}/{1594 + (i // 8) % 8}.pbf", n=100)

measure("map tile, raster (server draws it)", TILES, "GET",
        lambda i: f"/styles/basic-preview/12/{2078 + (i % 8)}/{1594 + (i // 8) % 8}.png",
        n=100)

measure("rider profile (authenticated read)", RIDER, "GET",
        lambda i: "/v2/profile", n=100, token=RTOK)

_t0 = datetime.now(timezone.utc)
measure("driver position update (write)", DRIVER, "POST",
        lambda i: "/ui/driver/location", n=100, token=DTOK,
        payload_fn=lambda i: [{"pt": {"lat": PICKUP[0] + 0.002, "lon": PICKUP[1] + 0.002},
                               "ts": (_t0 + timedelta(seconds=i)).isoformat()
                                     .replace("+00:00", "Z"), "acc": 8.0}])

SEARCH = {"fareProductType": "ONE_WAY",
          "contents": {"origin": {"address": {"area": "Alger Centre", "city": "Alger"},
                                  "gps": {"lat": PICKUP[0], "lon": PICKUP[1]}},
                       "destination": {"address": {"area": "Hussein Dey", "city": "Alger"},
                                       "gps": {"lat": DROP[0], "lon": DROP[1]}}}}
measure("RIDE REQUEST: price, route, ping drivers", RIDER, "POST",
        lambda i: "/v2/rideSearch", n=30, warm=3, token=RTOK,
        payload_fn=lambda i: SEARCH)

# ------------------------------------------------------------------- put back
if HOME:
    la, lo = HOME.split(",")
    pg(f"UPDATE atlas_driver_offer_bpp.driver_location SET lat={la}, lon={lo} "
       f"WHERE driver_id='{DID}'")

say("")
say("=" * 96)
say("WHAT THAT MEANS, if the whole box did nothing but this one thing")
say("=" * 96)
usable = CORES - IDLE * CORES
say(f"usable: {usable:.2f} cores after background work")
say("")
say(f"  {'operation':<42} {'per second':>12} {'per hour':>14}")
for r in RESULTS:
    if r["per_req"] <= 0:
        say(f"  {r['label']:<42} {'too cheap to measure':>27}")
        continue
    rps = usable / r["per_req"]
    say(f"  {r['label']:<42} {rps:>12,.0f} {rps*3600:>14,.0f}")

say("")
say("done " + datetime.now(timezone.utc).isoformat(timespec="seconds"))
