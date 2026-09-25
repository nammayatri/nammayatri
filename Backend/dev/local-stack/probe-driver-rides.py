#!/usr/bin/env python3
"""Does /rating/driver/{id} now carry a ride count, and is it the right one?

Two things are checked, and the second is the one that matters:

  1. the count matches `count(*)` over his COMPLETED rides, and
  2. a driver with rides but **no rating** still reports them.

(2) is where the previous version of this handler was wrong. It returned early
whenever `rating` was null, which would have hidden the ride count of every
driver nobody has rated yet -- exactly the drivers a passenger most needs a
second number for. A probe that only looked at rated drivers would have passed.

Run it ON the VPS.
"""
import json
import subprocess
import sys
import time
import urllib.error
import urllib.request

BASE = "https://api.169-58-139-65.sslip.io"


def get(path):
    q = urllib.request.Request(BASE + path, method="GET")
    try:
        with urllib.request.urlopen(q, timeout=20) as r:
            return json.loads(r.read().decode()), r.status
    except urllib.error.HTTPError as e:
        return {}, e.code
    except Exception:
        return {}, 0


# ── Paced, and that is a finding rather than a workaround ────────────────────
# The first run of this fired 33 requests as fast as it could and the last ten
# came back empty -- which looked exactly like a handler returning no `rides`
# key. It was nginx: `limit_req zone=api burst=20 nodelay` in front of
# /rating/, doing precisely its job.
#
# Worth knowing and worth not "fixing": the offer screen makes one request per
# offer and there are one to five offers, so nothing a passenger does comes
# near this. A probe walking every driver in the database is not a passenger.
PACE = 0.15


def psql(sql):
    r = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres", "-d", "atlas_dev", "-tAc", sql],
        capture_output=True, text=True, timeout=60)
    return r.stdout.strip()


rows = psql(
    "SELECT p.id || '|' || coalesce(p.first_name,'?') || '|' "
    "|| coalesce(p.rating::text,'null') || '|' "
    "|| (SELECT count(*) FROM atlas_driver_offer_bpp.ride r "
    "     WHERE r.driver_id = p.id AND r.status = 'COMPLETED') "
    "FROM atlas_driver_offer_bpp.person p WHERE p.role = 'DRIVER' "
    "ORDER BY 1;").splitlines()

ok = True
unrated_with_rides = 0
print(f"{'driver':<14} {'rating':<8} {'db rides':>8}  {'route rides':>11}  {'http':>5}")
print("-" * 60)

for line in rows:
    pid, name, rating, rides = line.split("|")
    time.sleep(PACE)
    body, code = get(f"/rating/driver/{pid}")
    got = body.get("rides")
    agrees = code == 200 and got == int(rides)
    ok &= agrees
    if rating == "null" and int(rides) > 0:
        unrated_with_rides += 1
    mark = "ok" if agrees else "FAIL"
    print(f"{name[:13]:<14} {rating:<8} {rides:>8}  {str(got):>11}  {code:>5}  {mark}")

print("-" * 52)
print(f"drivers with rides but no rating: {unrated_with_rides}")
if unrated_with_rides == 0:
    print("  ^ nobody in that state today, so the early-return bug could not be")
    print("    caught by this data alone. The handler no longer has one.")

print("\nALL AGREE" if ok else "\nSOMETHING IS WRONG")
sys.exit(0 if ok else 1)
