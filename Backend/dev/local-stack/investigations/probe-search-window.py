#!/usr/bin/env python3
"""Measure how long a driver actually gets to answer, end to end.

    ./probe-search-window.py            # on the VPS

Reads the window off a **real** request, the way the driver's phone reads it:
`searchRequestValidTill - startTime` on the row the dispatcher just wrote.

── Why this exists rather than trusting the config ─────────────────────────
`apply-search-window.sh` edits a Dhall value and restarts the container. Both
steps report success whether or not the running process picked the value up,
and the only place the truth appears is a row written by the next dispatch. The
window shipped at 10 s, was believed to be 16.3 s for a day on the strength of
one sample, and is the number an entire screen is built around -- so it gets
measured rather than assumed.

── It signs in as a RIDER and nothing else ────────────────────────────────
Deliberately. `probe-booking-flow.py` signs in as a driver, and on 16 August
that took the whole fleet down: one session per user, so the probe's login
revoked the daemon's, the daemon signed back in, and the two raced into
`HITS_LIMIT_EXCEED` together while `cmd_daemon`'s exit path put every driver
offline. A probe must never hold a fleet driver's session. This one publishes a
search and then reads Postgres -- the drivers involved never know it happened.

The rider number below is the probe's own and is not a fleet number.
"""
import json
import subprocess
import sys
import time
import urllib.error
import urllib.request
from datetime import datetime

RIDER_API = "http://localhost:8014"
MERCHANT = "YATRI"
OTP = "7891"
RIDER_NUM = "0555000199"

# Alger centre -> Hussein Dey. Any serviceable pair inside the geofence does;
# these two are the pair every other probe in this directory uses.
PICKUP = (36.7538, 3.0588)
DROP = (36.7370, 3.1150)


def call(method, url, body=None, token=None):
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(url, data=data, method=method)
    req.add_header("Content-Type", "application/json")
    if token:
        req.add_header("token", token)
    try:
        with urllib.request.urlopen(req, timeout=20) as r:
            return json.loads(r.read().decode() or "{}"), r.status
    except urllib.error.HTTPError as e:
        return e.read().decode()[:300], e.code
    except Exception as e:
        return str(e), 0


def pg(sql):
    out = subprocess.run(
        # No `-i`. With it, docker reads the same stdin this script may be fed
        # on and swallows the rest -- the trap from 12 August, met twice.
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres", "-d",
         "atlas_dev", "-tAc", sql],
        capture_output=True, text=True, timeout=30)
    return out.stdout.strip()


print("signing in as the probe rider")
a, code = call("POST", f"{RIDER_API}/v2/auth", {
    "mobileNumber": RIDER_NUM, "mobileCountryCode": "+213",
    "merchantId": MERCHANT})
if not isinstance(a, dict) or "authId" not in a:
    sys.exit(f"auth failed ({code}): {a}")

v, code = call("POST", f"{RIDER_API}/v2/auth/{a['authId']}/verify",
               {"otp": OTP, "deviceToken": "probe-window"})
token = v.get("token") if isinstance(v, dict) else None
if not token:
    sys.exit(f"verify failed ({code}): {v}")

# How many driver-requests existed before, so the new ones can be told apart
# from the 164 already in the table.
before = pg("SELECT count(*) FROM atlas_driver_offer_bpp.search_request_for_driver;")
print(f"  requests in the table before this search: {before}")

print("publishing a search")
s, code = call("POST", f"{RIDER_API}/v2/rideSearch", {
    "fareProductType": "ONE_WAY",
    "contents": {
        "origin": {"address": {"area": "Alger Centre", "city": "Alger"},
                   "gps": {"lat": PICKUP[0], "lon": PICKUP[1]}},
        "destination": {"address": {"area": "Hussein Dey", "city": "Alger"},
                        "gps": {"lat": DROP[0], "lon": DROP[1]}}}},
    token=token)
if not isinstance(s, dict) or "searchId" not in s:
    sys.exit(f"search failed ({code}): {s}")
print(f"  searchId={s['searchId']}")

# The rider must pick an estimate before the dispatcher writes anything: the
# batch job runs on `select`, not on `search`. Poll for the estimates, take any
# one, and select it -- which variant does not matter here, only that a request
# reaches some driver.
print("waiting for estimates")
est = None
for _ in range(15):
    r, _ = call("GET", f"{RIDER_API}/v2/rideSearch/{s['searchId']}/results",
                token=token)
    ests = r.get("estimates", []) if isinstance(r, dict) else []
    if ests:
        est = ests[0]
        print("  variants offered: " + ", ".join(
            f"{e.get('vehicleVariant')}={e.get('estimatedTotalFare')}" for e in ests))
        break
    time.sleep(2)

if not est:
    sys.exit("no estimates -- no driver of any variant is online and fresh.\n"
             "Run ./setup.sh drivers and ./fleet-service.sh, then try again.")

# `/select`, with **no body**. The running binary declares no request body on
# this route; `autoAssignEnabled` belongs to `/select2`, and anything posted
# here is dropped by Servant with a cheerful 200. Plain `/select` behaves as
# auto-assign off, which is what the app does and what we want measured.
call("POST", f"{RIDER_API}/v2/estimate/{est['id']}/select", None, token=token)

print("waiting for the dispatcher to write a driver request")
row = ""
for _ in range(15):
    row = pg("""SELECT EXTRACT(EPOCH FROM (search_request_valid_till - start_time))
                  FROM atlas_driver_offer_bpp.search_request_for_driver
                 ORDER BY created_at DESC LIMIT 1;""")
    after = pg("SELECT count(*) FROM atlas_driver_offer_bpp.search_request_for_driver;")
    if after != before:
        break
    time.sleep(2)
else:
    sys.exit("no new driver request was written.\n"
             "The pool found nobody -- stale positions, or no driver of the\n"
             "variant that was selected. This says nothing about the window.")

seconds = float(row)
print()
print(f"  MEASURED WINDOW: {seconds:.0f} s")
print()

# The spread over every request the dispatcher has ever written, which is how
# the 10 s figure was established in the first place and how a change is seen
# to have taken rather than merely been configured.
spread = pg("""SELECT EXTRACT(EPOCH FROM (search_request_valid_till - start_time))::int AS w,
                      count(*)
                 FROM atlas_driver_offer_bpp.search_request_for_driver
                GROUP BY 1 ORDER BY 1;""")
print("every window this database has recorded:")
for line in spread.splitlines():
    w, n = line.split("|")
    print(f"  {w:>4} s   x{n}")

print()
print(f"read at {datetime.now().isoformat(timespec='seconds')}")
