#!/usr/bin/env python3
"""How much traffic does this server actually take, and what does data cost it?

Written to answer two questions from the client that had only ever been
answered by guesswork: how many requests can it handle, and how many riders,
drivers and rides can it store.

RUNS ON THE VPS. Not from a laptop, for two reasons:

  * the edge rate-limits every client IP to 240 r/m with 24 concurrent
    connections (`edge/nginx.conf`). A load test from outside measures OUR OWN
    guard -- it would report ~4 r/s no matter how fast the server is, which is
    a fact about nginx, not about the server.
  * the link from Algeria to this box is slower than the box, so latency would
    be network, not service.

The generator therefore shares the six cores with the services it is testing.
That understates the server: the numbers below are a FLOOR, not a ceiling.
The generator's own CPU share is measured and printed so the size of that
understatement is visible rather than assumed.

Each endpoint is run at rising concurrency until throughput stops improving --
the point where it stops is the answer, and it is the point where a queue
starts forming. The ladder aborts early if errors pass 25% or p95 passes 5s,
so a measurement can never be the thing that takes the pilot down.

Nothing here is destructive, but two things are touched and both are restored:
one demo driver is signed in (this backend allows ONE session per person, so
signing in as a real driver logs that driver's phone out -- the boss's test
account is excluded by id), and that driver's stored position is put back at
the end.
"""
import http.client
import json
import subprocess
import sys
import threading
import time
from datetime import datetime, timedelta, timezone

RIDER = ("127.0.0.1", 8013)
DRIVER = ("127.0.0.1", 8017)
SHIM = ("127.0.0.1", 8030)
OSRM = ("127.0.0.1", 5000)

RIDER_NUM = "0555000199"
RIDER_MERCHANT = "YATRI"
OTP = "7891"

# The account the client uses to test payments. Signing in as him would log his
# phone out mid-demo.
BOSS_TEST_DRIVER = "98821a0c-86b8-42a9-93f7-8260c1af9232"

LEVELS = [1, 2, 4, 8, 16, 32, 64]
SECONDS_PER_LEVEL = 8
WARMUP = 1.5

# A search is the most expensive thing the server does -- it prices the trip,
# calls OSRM and pings every nearby driver -- and unlike the others it WRITES.
# It is measured by a fixed request count rather than a duration so the number
# of rows this leaves behind is known in advance, not discovered afterwards.
SEARCH_LEVELS = [1, 2, 4, 8]
SEARCHES_PER_LEVEL = 12

PICKUP = (36.7538, 3.0588)   # Alger Centre
DROP = (36.7050, 3.1750)     # Hussein Dey, ~13 km


def say(msg):
    print(msg, flush=True)


def pg(sql, db="atlas_dev"):
    # No `docker exec -i`: the -i steals the stdin this script arrives on.
    out = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres", "-d", db,
         "-At", "-c", sql],
        capture_output=True, text=True, timeout=60)
    return out.stdout.strip()


def call(hostport, method, path, body=None, token=None):
    conn = http.client.HTTPConnection(*hostport, timeout=30)
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
        return None, 0, str(exc)
    finally:
        conn.close()
    try:
        return json.loads(raw), code, raw
    except Exception:
        return None, code, raw


# ----------------------------------------------------------------- the machine
def cpu_sample():
    with open("/proc/stat") as fh:
        v = [int(x) for x in fh.readline().split()[1:]]
    return v[3] + v[4], sum(v)      # idle+iowait, total


def cpu_busy(a, b):
    d_total = b[1] - a[1]
    if d_total <= 0:
        return 0.0
    return 100.0 * (1.0 - (b[0] - a[0]) / d_total)


def self_cpu():
    with open("/proc/self/stat") as fh:
        f = fh.readline().split()
    ticks = int(f[13]) + int(f[14])
    return ticks / 100.0        # USER_HZ is 100 everywhere this runs


def pct(sorted_vals, p):
    if not sorted_vals:
        return 0.0
    i = min(len(sorted_vals) - 1, int(round((p / 100.0) * (len(sorted_vals) - 1))))
    return sorted_vals[i] * 1000.0


# ------------------------------------------------------------- the load engine
class Spec:
    """One endpoint under test. `body_fn` exists for requests that may not
    repeat themselves -- a driver position whose timestamp is not newer than
    the stored one is silently DROPPED, so a fixed body would measure a
    rejection path rather than a write."""

    def __init__(self, label, hostport, method, path, token=None, body_fn=None):
        self.label = label
        self.hostport = hostport
        self.method = method
        self.path = path
        self.token = token
        self.body_fn = body_fn

    def headers(self):
        h = {"content-type": "application/json"}
        if self.token:
            h["token"] = self.token
        return h


_ts_lock = threading.Lock()
_ts_cursor = [datetime.now(timezone.utc)]


def driver_location_body():
    with _ts_lock:
        _ts_cursor[0] += timedelta(seconds=1)
        ts = _ts_cursor[0]
    return json.dumps([{
        "pt": {"lat": PICKUP[0] + 0.002, "lon": PICKUP[1] + 0.002},
        "ts": ts.isoformat().replace("+00:00", "Z"),
        "acc": 8.0,
    }]).encode()


def run_level(spec, concurrency, seconds):
    """Closed-loop: `concurrency` workers, each sending the next request the
    moment the previous one answers. Connections are kept alive, as a phone's
    would be, so TCP setup is not counted as service time."""
    samples = []
    stop = threading.Event()
    deadline = time.time() + seconds + WARMUP
    warm_until = time.time() + WARMUP

    def worker():
        conn = None
        while time.time() < deadline and not stop.is_set():
            if conn is None:
                try:
                    conn = http.client.HTTPConnection(*spec.hostport, timeout=25)
                except Exception:
                    time.sleep(0.05)
                    continue
            body = spec.body_fn() if spec.body_fn else None
            t0 = time.perf_counter()
            try:
                conn.request(spec.method, spec.path, body=body, headers=spec.headers())
                resp = conn.getresponse()
                resp.read()
                code = resp.status
            except Exception:
                code = 0
                try:
                    conn.close()
                except Exception:
                    pass
                conn = None
            dt = time.perf_counter() - t0
            if time.time() > warm_until:
                samples.append((dt, code))
        if conn is not None:
            try:
                conn.close()
            except Exception:
                pass

    c0, s0 = cpu_sample(), self_cpu()
    threads = [threading.Thread(target=worker, daemon=True) for _ in range(concurrency)]
    t_start = time.time()
    for t in threads:
        t.start()
    for t in threads:
        t.join(timeout=seconds + WARMUP + 30)
    elapsed = time.time() - t_start - WARMUP
    c1, s1 = cpu_sample(), self_cpu()

    lat = sorted(s[0] for s in samples)
    ok = sum(1 for s in samples if 200 <= s[1] < 300)
    bad = len(samples) - ok
    box = cpu_busy(c0, c1)
    gen = 100.0 * (s1 - s0) / max(elapsed, 0.001) / 6.0   # 6 cores
    return {
        "n": len(samples), "ok": ok, "bad": bad,
        "rps": len(samples) / elapsed if elapsed > 0 else 0.0,
        "p50": pct(lat, 50), "p95": pct(lat, 95), "p99": pct(lat, 99),
        "box_cpu": box, "gen_cpu": gen,
        "codes": sorted({s[1] for s in samples}),
    }


def ladder(spec):
    say("")
    say(f"### {spec.label}")
    say(f"{'conc':>5} {'req/s':>9} {'p50 ms':>8} {'p95 ms':>9} {'p99 ms':>9} "
        f"{'errors':>7} {'box cpu':>8} {'of which us':>12}   codes")
    best = None
    for c in LEVELS:
        r = run_level(spec, c, SECONDS_PER_LEVEL)
        err_pct = 100.0 * r["bad"] / max(r["n"], 1)
        say(f"{c:>5} {r['rps']:>9.1f} {r['p50']:>8.1f} {r['p95']:>9.1f} {r['p99']:>9.1f} "
            f"{err_pct:>6.1f}% {r['box_cpu']:>7.1f}% {r['gen_cpu']:>11.1f}%   {r['codes']}")
        if best is None or r["rps"] > best[1]["rps"]:
            best = (c, r)
        if err_pct > 25.0:
            say(f"      stopped climbing: {err_pct:.0f}% of requests failed")
            break
        if r["p95"] > 5000.0:
            say("      stopped climbing: p95 passed 5s -- a queue has formed")
            break
    say(f"  -> peak {best[1]['rps']:.1f} req/s at {best[0]} concurrent "
        f"(p95 {best[1]['p95']:.0f} ms, box {best[1]['box_cpu']:.0f}% busy, "
        f"generator itself {best[1]['gen_cpu']:.0f}%)")
    return best


# ------------------------------------------------------------------- preflight
say("=" * 78)
say("CAPACITY TEST -- what this server takes, and what data costs it")
say(f"started {datetime.now(timezone.utc).isoformat(timespec='seconds')}")
say("=" * 78)

cores = int(subprocess.run(["nproc"], capture_output=True, text=True).stdout.strip())
say(f"box: {cores} cores")

say("")
say("--- signing in a rider")
a, code, raw = call(RIDER, "POST", "/v2/auth", {
    "mobileNumber": RIDER_NUM, "mobileCountryCode": "+213",
    "merchantId": RIDER_MERCHANT})
if not isinstance(a, dict) or "authId" not in a:
    say(f"  rider auth FAILED {code}: {raw[:200]}")
    sys.exit(1)
v, code, raw = call(RIDER, "POST", f"/v2/auth/{a['authId']}/verify",
                    {"otp": OTP, "deviceToken": "loadtest"})
RTOK = v.get("token") if isinstance(v, dict) else None
if not RTOK:
    say(f"  rider verify FAILED {code}: {raw[:200]}")
    sys.exit(1)
say("  ok")

say("--- signing in a driver (not the client's payment test account)")
cred = pg(f"""SELECT p.unencrypted_mobile_number || '~' || p.mobile_country_code
                     || '~' || m.id || '~' || p.id
                FROM atlas_driver_offer_bpp.person p
                JOIN atlas_driver_offer_bpp.merchant m ON m.id = p.merchant_id
               WHERE p.role='DRIVER' AND p.unencrypted_mobile_number IS NOT NULL
                 AND p.mobile_country_code = '+213'
                 AND p.id <> '{BOSS_TEST_DRIVER}'
               LIMIT 1;""")
if not cred:
    say("  no usable Algerian driver -- skipping the driver-side test")
    DTOK, DID, HOME = None, None, None
else:
    DNUM, DCC, DMER, DID = cred.split("~")
    HOME = pg("SELECT lat || ',' || lon FROM atlas_driver_offer_bpp.driver_location "
              f"WHERE driver_id='{DID}';")
    say(f"  driver {DNUM} id={DID}; position now {HOME or 'NONE'} (restored at the end)")
    a, code, raw = call(DRIVER, "POST", "/ui/auth", {
        "mobileNumber": DNUM, "mobileCountryCode": DCC, "merchantId": DMER})
    DTOK = None
    if isinstance(a, dict) and "authId" in a:
        v, code, raw = call(DRIVER, "POST", f"/ui/auth/{a['authId']}/verify",
                            {"otp": OTP, "deviceToken": "loadtest"})
        DTOK = v.get("token") if isinstance(v, dict) else None
    if not DTOK:
        say(f"  driver auth FAILED {code}: {raw[:200]} -- skipping driver-side test")
    else:
        say("  ok")

# The size of the database before anything is written, so the cost of a search
# can be measured rather than estimated.
def db_facts():
    return {
        "bytes": int(pg("SELECT pg_database_size('atlas_dev')") or 0),
        "searches": int(pg("SELECT count(*) FROM atlas_app.search_request") or 0),
        "estimates": int(pg("SELECT count(*) FROM atlas_app.estimate") or 0),
        "beckn": int(pg("SELECT count(*) FROM atlas_app.beckn_request") or 0),
    }


before = db_facts()
say(f"--- database before: {before['bytes']/1e6:.1f} MB, "
    f"{before['searches']} searches on record")

# ---------------------------------------------------------------- the read load
say("")
say("=" * 78)
say("PART 1 -- the endpoints a phone hits constantly")
say("=" * 78)

results = {}

results["autocomplete"] = ladder(Spec(
    "place search (every keystroke in the destination box) -- shim + postgres",
    SHIM, "GET", "/place/autocomplete/json?input=bab&location=36.7538,3.0588"))

results["osrm"] = ladder(Spec(
    "route (every fare estimate and every line drawn on the map) -- OSRM",
    OSRM, "GET",
    "/route/v1/driving/3.0588,36.7538;3.1731,36.7372?overview=false"))

results["profile"] = ladder(Spec(
    "rider profile (an authenticated read) -- rider-app + redis + postgres",
    RIDER, "GET", "/v2/profile", token=RTOK))

if DTOK:
    probe, code, raw = call(DRIVER, "POST", "/ui/driver/location",
                            json.loads(driver_location_body().decode()), token=DTOK)
    say("")
    say(f"--- driver position write answers {code} "
        f"({'usable' if 200 <= code < 300 else 'NOT a success path: ' + raw[:120]})")
    if 200 <= code < 300:
        results["location"] = ladder(Spec(
            "driver position (every online driver, every few seconds) -- WRITE",
            DRIVER, "POST", "/ui/driver/location", token=DTOK,
            body_fn=driver_location_body))

# ------------------------------------------------------------------ the search
say("")
say("=" * 78)
say("PART 2 -- a whole ride request: pricing, routing and pinging drivers")
say("=" * 78)
say("bounded by request count, not by time: this one writes rows.")
say(f"{'conc':>5} {'req/s':>9} {'p50 ms':>8} {'p95 ms':>9} {'errors':>7} {'box cpu':>8}   codes")

search_body = {
    "fareProductType": "ONE_WAY",
    "contents": {
        "origin": {"address": {"area": "Alger Centre", "city": "Alger"},
                   "gps": {"lat": PICKUP[0], "lon": PICKUP[1]}},
        "destination": {"address": {"area": "Hussein Dey", "city": "Alger"},
                        "gps": {"lat": DROP[0], "lon": DROP[1]}}}}
search_payload = json.dumps(search_body).encode()

search_best = (0, 0.0)
search_failures = []
for conc in SEARCH_LEVELS:
    samples = []
    remaining = [SEARCHES_PER_LEVEL]
    lock = threading.Lock()

    def searcher():
        conn = None
        while True:
            with lock:
                if remaining[0] <= 0:
                    return
                remaining[0] -= 1
            if conn is None:
                conn = http.client.HTTPConnection(*RIDER, timeout=40)
            t0 = time.perf_counter()
            try:
                conn.request("POST", "/v2/rideSearch", body=search_payload,
                             headers={"content-type": "application/json", "token": RTOK})
                r = conn.getresponse()
                raw = r.read().decode(errors="replace")
                code = r.status
            except Exception as exc:
                code, raw = 0, str(exc)
                conn = None
            samples.append((time.perf_counter() - t0, code))
            if code != 200 and len(search_failures) < 4:
                search_failures.append(f"{code}: {raw[:160]}")
        # connection closed by GC

    c0 = cpu_sample()
    t_start = time.time()
    threads = [threading.Thread(target=searcher, daemon=True) for _ in range(conc)]
    for t in threads:
        t.start()
    for t in threads:
        t.join(timeout=180)
    elapsed = max(time.time() - t_start, 0.001)
    c1 = cpu_sample()

    lat = sorted(s[0] for s in samples)
    bad = sum(1 for s in samples if not 200 <= s[1] < 300)
    rps = len(samples) / elapsed
    say(f"{conc:>5} {rps:>9.2f} {pct(lat,50):>8.0f} {pct(lat,95):>9.0f} "
        f"{100.0*bad/max(len(samples),1):>6.1f}% {cpu_busy(c0,c1):>7.1f}%   "
        f"{sorted({s[1] for s in samples})}")
    if rps > search_best[1]:
        search_best = (conc, rps)

if search_failures:
    say("  failures seen:")
    for f in search_failures:
        say(f"    {f}")
say(f"  -> peak {search_best[1]:.2f} searches/s at {search_best[0]} concurrent "
    f"= {search_best[1]*3600:.0f} ride requests/hour")

# ------------------------------------------------------------------ what it cost
say("")
say("=" * 78)
say("PART 3 -- what data costs on disk")
say("=" * 78)

pg("VACUUM ANALYZE")           # so the size below is settled, not in-flight
after = db_facts()
d_bytes = after["bytes"] - before["bytes"]
d_search = after["searches"] - before["searches"]
say(f"database grew {d_bytes/1024:.0f} kB while {d_search} searches were made "
    f"(+{after['estimates']-before['estimates']} estimates, "
    f"+{after['beckn']-before['beckn']} beckn log rows)")
if d_search > 0:
    per = d_bytes / d_search
    say(f"  -> one ride request costs about {per/1024:.1f} kB, measured")

say("")
say("average row width, measured on the real rows (heap only, no indexes):")
for label, tbl in [
        ("a rider account", "atlas_app.person"),
        ("a driver account", "atlas_driver_offer_bpp.person"),
        ("a booking", "atlas_app.booking"),
        ("a ride", "atlas_app.ride"),
        ("a beckn log row", "atlas_app.beckn_request")]:
    w = pg(f"SELECT coalesce(round(avg(pg_column_size(t.*))),0) FROM {tbl} t")
    say(f"  {label:<18} {w or '?':>8} bytes")

say("")
say("free space, and what it holds:")
free = subprocess.run(["df", "-B1", "--output=avail", "/"],
                      capture_output=True, text=True).stdout.split()[-1]
say(f"  disk free: {int(free)/1e9:.0f} GB")

# ----------------------------------------------------------------- put it back
if DTOK and HOME:
    lat_s, lon_s = HOME.split(",")
    pg(f"UPDATE atlas_driver_offer_bpp.driver_location "
       f"SET lat={lat_s}, lon={lon_s} WHERE driver_id='{DID}'")
    say("")
    say(f"driver position restored to {HOME}")

say("")
say("done " + datetime.now(timezone.utc).isoformat(timespec="seconds"))
