#!/usr/bin/env python3
"""Does the deployed binary actually honour the restriction list?

Run it ON the VPS, after deploying binaries built with the dispatch patch:

    python3 probe-dispatch-restriction.py

── Why this is the acceptance test for the whole feature ───────────────────
`probe-restricted-drivers.py` proves the list is right. This proves the binary
reads it. Between those two is a Redis key nobody validates: if its name is
wrong by one character the binary finds nothing, restricts nobody, and every
other test in the project still passes. The feature would be silently off.

── What it looks at ────────────────────────────────────────────────────────
Dispatch happens at *search* time, and it writes down who it asked:
`search_request_for_driver` carries one row per driver contacted. So a search
plus that table is the whole observation — no booking, no driver responses.

── The two cases, and the second is the client's actual rule ───────────────
    some drivers restricted -> the restricted ones are not asked
    ALL drivers restricted  -> everyone is asked anyway

The second is "he still gets a request when he is the only one in the area",
stated in the only way that can be tested with a fleet parked on one street.
Getting it backwards would silence an unpaid driver completely, which is not
what the client asked for and is the failure he would hear about.

Everything is restored, including the Redis key.
"""
import json
import subprocess
import sys
import time
import urllib.request
from datetime import datetime, timezone

R = "http://localhost:8013"
KEY = "dynamic-offer-driver-app:movin:restricted"
OTP = "7891"
RIDER = "0555000199"
MERCHANT = "YATRI"
PICKUP = (36.7538, 3.0588)
DROP = (36.7050, 3.1750)

passed = 0
failed = 0


def check(name, ok, detail=""):
    global passed, failed
    if ok:
        passed += 1
        print(f"  PASS  {name}")
    else:
        failed += 1
        print(f"  FAIL  {name}   {detail}")


def call(method, url, body=None, token=None):
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(url, data=data, method=method)
    req.add_header("Content-Type", "application/json")
    if token:
        req.add_header("token", token)
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            return json.loads(r.read().decode() or "{}")
    except Exception as e:                                   # noqa: BLE001
        print(f"    ! {method} {url} -> {e}")
        return {}


def psql(sql):
    r = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres",
         "-d", "atlas_dev", "-tAc", sql],
        capture_output=True, text=True, timeout=60)
    return r.stdout.strip()


def redis_set(value):
    subprocess.run(["docker", "exec", "ny-redis", "redis-cli", "SET", KEY, value],
                   capture_output=True, text=True, timeout=30)


def redis_get():
    r = subprocess.run(["docker", "exec", "ny-redis", "redis-cli", "GET", KEY],
                       capture_output=True, text=True, timeout=30)
    return r.stdout.strip()


def sign_in():
    a = call("POST", f"{R}/v2/auth", {
        "mobileNumber": RIDER, "mobileCountryCode": "+213", "merchantId": MERCHANT})
    if "authId" not in a:
        return None
    v = call("POST", f"{R}/v2/auth/{a['authId']}/verify",
             {"otp": OTP, "deviceToken": "probe-dispatch"})
    return v.get("token")


def who_was_asked(token):
    """Search, then read the drivers dispatch wrote down."""
    call("POST", f"{R}/v2/rideSearch", {
        "fareProductType": "ONE_WAY",
        "contents": {
            "origin": {"address": {"area": "Alger Centre", "city": "Alger"},
                       "gps": {"lat": PICKUP[0], "lon": PICKUP[1]}},
            "destination": {"address": {"area": "Hussein Dey", "city": "Alger"},
                            "gps": {"lat": DROP[0], "lon": DROP[1]}}}},
        token=token)
    # Dispatch is 0.25-0.49 s at search time; a second is generous.
    time.sleep(2.0)
    rows = psql(
        "SELECT DISTINCT driver_id FROM atlas_driver_offer_bpp.search_request_for_driver "
        "WHERE created_at > now() - interval '30 seconds'")
    return {x for x in rows.split("\n") if x}


D = "http://localhost:8017"


def put_on_duty(n=2):
    """Sign drivers in, park them by the pickup, mark them active.

    The probe used to read whoever looked online in the database and search.
    That found nobody, repeatedly, while the booking-flow probe working from the
    same fleet got four estimates -- because the flow probe signs its own driver
    in and this one inherited whatever the last probe left behind. A test that
    depends on ambient state is a test that fails for reasons that are not the
    thing under test.

    +213 only: the other seeded drivers are upstream's Indian and Sri Lankan
    ones and the country-code validator rejects them.
    """
    rows = psql(
        "SELECT p.unencrypted_mobile_number || '~' || p.mobile_country_code "
        "    || '~' || p.merchant_id || '~' || p.id "
        "  FROM atlas_driver_offer_bpp.person p "
        "  JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id "
        "  JOIN atlas_driver_offer_bpp.vehicle v ON v.driver_id = p.id "
        " WHERE p.role = 'DRIVER' AND p.mobile_country_code = '+213' "
        "   AND p.unencrypted_mobile_number IS NOT NULL "
        "   AND di.enabled AND NOT di.blocked "
        " ORDER BY p.id LIMIT %d" % n)
    on = []
    for row in [r for r in rows.split("\n") if r]:
        num, cc, mer, did = row.split("~")
        a = call("POST", f"{D}/ui/auth",
                 {"mobileNumber": num, "mobileCountryCode": cc, "merchantId": mer})
        if "authId" not in a:
            continue
        v = call("POST", f"{D}/ui/auth/{a['authId']}/verify",
                 {"otp": OTP, "deviceToken": "probe-dispatch"})
        tok = v.get("token")
        if not tok:
            continue
        now = datetime.now(timezone.utc).isoformat().replace("+00:00", "Z")
        # Spread them slightly so they are not one point on the map.
        off = 0.002 * (len(on) + 1)
        call("POST", f"{D}/ui/driver/location",
             [{"pt": {"lat": PICKUP[0] + off, "lon": PICKUP[1] + off},
               "ts": now, "acc": 8.0}], token=tok)
        call("POST", f"{D}/ui/driver/setActivity?active=true", None, token=tok)
        on.append(did)
    return on


# ── the fleet, and a clean slate ────────────────────────────────────────────
print("== putting drivers on duty")
fleet = put_on_duty(1)
print(f"   {len(fleet)} driver(s) signed in and active")
if not fleet:
    # 429 here is our own OTP guard, which fires on the third attempt and is
    # shared across every probe that signs anybody in. It is not a failure of
    # the thing under test; it means this probe was run too often. Wait a
    # minute rather than reading it as a broken deploy.
    print("   could not put a driver on duty (429 = the OTP guard; wait a minute)")
    sys.exit(2)

before_key = redis_get()
token = sign_in()
if not token:
    print("   rider sign-in failed")
    sys.exit(2)

try:
    # ── nobody restricted: who does dispatch normally reach ─────────────────
    print("\n== with nobody restricted")
    redis_set("[]")
    baseline = who_was_asked(token)
    print(f"   dispatch asked {len(baseline)} driver(s)")
    check("dispatch asks somebody at all", len(baseline) > 0,
          "no rows in search_request_for_driver — is the fleet online and fresh?")
    if not baseline:
        raise SystemExit(1)

    # ── one of them restricted ──────────────────────────────────────────────
    victim = sorted(baseline)[0]
    print(f"\n== with {victim} restricted")
    redis_set(json.dumps([victim]))
    asked = who_was_asked(token)
    print(f"   dispatch asked {len(asked)} driver(s)")
    check("the restricted driver is not asked", victim not in asked,
          "he was asked anyway — the binary is not reading the key")
    # Only meaningful when somebody else was in the pool to begin with.
    if len(baseline) > 1:
        check("the others still are", len(asked) > 0,
              "restricting one silenced everybody")
    else:
        print("  ----  only one driver in the pool; skipping the others check")

    # ── all of them restricted: the client's actual rule ────────────────────
    print("\n== with EVERY driver restricted")
    redis_set(json.dumps(sorted(fleet)))
    asked_all = who_was_asked(token)
    print(f"   dispatch asked {len(asked_all)} driver(s)")
    check("everyone is still asked when nobody has paid", len(asked_all) > 0,
          "the pool went empty — an unpaid driver would never work again")

finally:
    redis_set(before_key if before_key else "[]")
    print(f"\n   restored the key to {redis_get()}")

print(f"\n{passed} passed, {failed} failed")
sys.exit(1 if failed else 0)
