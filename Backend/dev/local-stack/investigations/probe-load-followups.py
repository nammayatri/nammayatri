#!/usr/bin/env python3
"""Three loose ends from the service-time run, each of which changes the answer.

1. The driver position write answered non-2xx TWELVE times out of twelve. The
   throughput figure derived from it is therefore the cost of a REJECTION, not
   of a write, and is worthless until the actual status code is known.

2. The raster tile's slowest request was 3.07s and its median 215ms. That gap
   is the shape of a cold start -- fonts, sprites and the style document being
   loaded once. Capacity should be read off the warm figure, so the two have
   to be separated instead of averaged together.

3. One ride request appeared to cost 387 kB of disk, measured across only six
   of them. At six samples that number is mostly noise from Postgres extending
   files in 8 kB pages. More samples, and the number settles.

Sequential throughout. Nothing here runs two requests at once.
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
TILES = ("127.0.0.1", 8035)

RIDER_NUM = "0555000199"
RIDER_MERCHANT = "YATRI"
OTP = "7891"
BOSS_TEST_DRIVER = "98821a0c-86b8-42a9-93f7-8260c1af9232"
PICKUP = (36.7538, 3.0588)
DROP = (36.7050, 3.1750)

SEARCH_SAMPLES = 40


def say(m):
    print(m, flush=True)


def pg(sql, db="atlas_dev"):
    out = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres", "-d", db,
         "-At", "-c", sql],
        capture_output=True, text=True, timeout=120)
    return out.stdout.strip()


def call(hostport, method, path, body=None, token=None):
    conn = http.client.HTTPConnection(*hostport, timeout=40)
    headers = {"content-type": "application/json"}
    if token:
        headers["token"] = token
    data = json.dumps(body).encode() if body is not None else None
    try:
        conn.request(method, path, body=data, headers=headers)
        r = conn.getresponse()
        raw = r.read().decode(errors="replace")
        return r.status, raw
    except Exception as exc:
        return 0, str(exc)
    finally:
        conn.close()


say("=" * 80)
say("FOLLOW-UPS")
say("=" * 80)

# ------------------------------------------------------ 1. the failing write
say("")
say("--- 1. why did every driver position write fail?")
cred = pg(f"""SELECT p.unencrypted_mobile_number || '~' || p.mobile_country_code
                     || '~' || m.id || '~' || p.id
                FROM atlas_driver_offer_bpp.person p
                JOIN atlas_driver_offer_bpp.merchant m ON m.id = p.merchant_id
               WHERE p.role='DRIVER' AND p.unencrypted_mobile_number IS NOT NULL
                 AND p.mobile_country_code = '+213'
                 AND p.id <> '{BOSS_TEST_DRIVER}'
               LIMIT 1;""")
DNUM, DCC, DMER, DID = cred.split("~")
HOME = pg("SELECT lat || ',' || lon FROM atlas_driver_offer_bpp.driver_location "
          f"WHERE driver_id='{DID}';")
code, raw = call(DRIVER, "POST", "/ui/auth",
                 {"mobileNumber": DNUM, "mobileCountryCode": DCC, "merchantId": DMER})
authId = json.loads(raw)["authId"]
code, raw = call(DRIVER, "POST", f"/ui/auth/{authId}/verify",
                 {"otp": OTP, "deviceToken": "followup"})
DTOK = json.loads(raw)["token"]

now = datetime.now(timezone.utc)
body = [{"pt": {"lat": PICKUP[0] + 0.002, "lon": PICKUP[1] + 0.002},
         "ts": now.isoformat().replace("+00:00", "Z"), "acc": 8.0}]
code, raw = call(DRIVER, "POST", "/ui/driver/location", body, token=DTOK)
say(f"  offline driver, position post -> {code}: {raw[:220]}")

say("  marking him on duty and trying again")
code, raw = call(DRIVER, "POST", "/ui/driver/setActivity?active=true", None, token=DTOK)
say(f"  setActivity -> {code}: {raw[:120]}")
body[0]["ts"] = (now + timedelta(seconds=5)).isoformat().replace("+00:00", "Z")
code, raw = call(DRIVER, "POST", "/ui/driver/location", body, token=DTOK)
say(f"  on-duty driver, position post -> {code}: {raw[:220]}")

LOC_OK = 200 <= code < 300
if LOC_OK:
    say("  timing the real write path, 20 sequential posts")
    times = []
    for i in range(20):
        body[0]["ts"] = (now + timedelta(seconds=10 + i)).isoformat().replace("+00:00", "Z")
        t0 = time.perf_counter()
        c, _ = call(DRIVER, "POST", "/ui/driver/location", body, token=DTOK)
        times.append((time.perf_counter() - t0) * 1000.0)
    say(f"  median {statistics.median(times):.1f} ms   "
        f"min {min(times):.1f}   max {max(times):.1f}")

# Put him back exactly as found.
call(DRIVER, "POST", "/ui/driver/setActivity?active=false", None, token=DTOK)
if HOME:
    la, lo = HOME.split(",")
    pg("UPDATE atlas_driver_offer_bpp.driver_location "
       f"SET lat={la}, lon={lo} WHERE driver_id='{DID}'")
    say(f"  driver put back: off duty, position {HOME}")

# ------------------------------------------------------- 2. tiles, warm vs cold
say("")
say("--- 2. raster tiles: is 215 ms the steady state or the warm-up?")
for label, path in [("raster PNG (server renders)", "/styles/basic-preview/12/2082/1597.png"),
                    ("vector PBF (phone renders)", "/data/v3/12/2082/1597.pbf")]:
    conn = http.client.HTTPConnection(*TILES, timeout=60)
    times, sizes = [], []
    # 40 DIFFERENT tiles: asking for the same one 40 times measures a cache,
    # not the renderer. A phone panning the map asks for new squares.
    for i in range(40):
        x, y = 2078 + (i % 8), 1594 + (i // 8)
        p = path.replace("2082", str(x)).replace("1597", str(y))
        t0 = time.perf_counter()
        try:
            conn.request("GET", p)
            r = conn.getresponse()
            data = r.read()
            code = r.status
        except Exception:
            conn = http.client.HTTPConnection(*TILES, timeout=60)
            code, data = 0, b""
        times.append((time.perf_counter() - t0) * 1000.0)
        sizes.append(len(data))
    conn.close()
    first, rest = times[0], sorted(times[1:])
    say(f"  {label:<30} first {first:>7.0f} ms | then median "
        f"{statistics.median(rest):>6.1f} ms, p95 {rest[int(0.95*len(rest))]:>6.1f} ms, "
        f"max {max(rest):>7.1f} ms | avg {statistics.mean(sizes)/1024:.0f} kB")

# --------------------------------------------- 3. what a ride request really costs
say("")
say(f"--- 3. disk cost of a ride request, over {SEARCH_SAMPLES} samples this time")
pg("VACUUM ANALYZE")
b_bytes = int(pg("SELECT pg_database_size('atlas_dev')"))
b_rows = int(pg("SELECT count(*) FROM atlas_app.search_request"))

search_body = {
    "fareProductType": "ONE_WAY",
    "contents": {
        "origin": {"address": {"area": "Alger Centre", "city": "Alger"},
                   "gps": {"lat": PICKUP[0], "lon": PICKUP[1]}},
        "destination": {"address": {"area": "Hussein Dey", "city": "Alger"},
                        "gps": {"lat": DROP[0], "lon": DROP[1]}}}}

code, raw = call(RIDER, "POST", "/v2/auth", {
    "mobileNumber": RIDER_NUM, "mobileCountryCode": "+213",
    "merchantId": RIDER_MERCHANT})
authId = json.loads(raw)["authId"]
code, raw = call(RIDER, "POST", f"/v2/auth/{authId}/verify",
                 {"otp": OTP, "deviceToken": "followup"})
RTOK = json.loads(raw)["token"]

times, bad = [], {}
for i in range(SEARCH_SAMPLES):
    t0 = time.perf_counter()
    c, raw = call(RIDER, "POST", "/v2/rideSearch", search_body, token=RTOK)
    times.append((time.perf_counter() - t0) * 1000.0)
    if not 200 <= c < 300:
        bad[c] = bad.get(c, 0) + 1
pg("VACUUM ANALYZE")
a_bytes = int(pg("SELECT pg_database_size('atlas_dev')"))
a_rows = int(pg("SELECT count(*) FROM atlas_app.search_request"))

n = a_rows - b_rows
say(f"  {SEARCH_SAMPLES} requests sent, {n} search rows created, failures {bad or 'none'}")
say(f"  latency median {statistics.median(times):.0f} ms, "
    f"min {min(times):.0f}, max {max(times):.0f}")
if n > 0:
    say(f"  database grew {(a_bytes-b_bytes)/1024:.0f} kB "
        f"-> {(a_bytes-b_bytes)/n/1024:.1f} kB per ride request")

say("")
say("--- where that space goes")
say(pg("""SELECT c.relname || '  ' || pg_size_pretty(pg_total_relation_size(c.oid))
            FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace
           WHERE c.relkind='r' AND n.nspname IN ('atlas_app','atlas_driver_offer_bpp')
           ORDER BY pg_total_relation_size(c.oid) DESC LIMIT 8"""))

say("")
say("done " + datetime.now(timezone.utc).isoformat(timespec="seconds"))
