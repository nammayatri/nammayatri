#!/usr/bin/env python3
"""« Signaler » end to end: a real passenger token, through the public edge.

What the app does when a passenger reports his driver from the in-ride
screen: POST https://api.movinapp.net/rider/report with his session token and
{bookingId, text}. nginx sends it to admin-api, which asks the rider app whose
token it is, finds the booking only if it is his, and takes the driver from
the ride. Written 2026-09-27 with the feature.

RUNS ON THE VPS: the sign-in goes to the rider backend on loopback 8013, with
the backend's fixed test code, so no SMS is spent. The report itself goes
through the public hostname, because that is the path the phone takes.

One sign-in only (the backend's own auth limit fires on the third). The rows
this writes are deleted at the end, so the console's Signalements queue is
left as it was found.
"""
import json
import subprocess
import sys
import time
import urllib.error
import urllib.request

RIDER = "http://localhost:8013"
EDGE = "https://api.movinapp.net/rider/report"
NUM, MERCHANT, OTP = "0555000199", "YATRI", "7891"
OTHER = "0555000188"  # another probe passenger, with a ride of his own
PASS = FAIL = 0


def check(ok, what, detail=""):
    global PASS, FAIL
    if ok:
        PASS += 1
        print(f"  \033[1;32mok  \033[0m{what}")
    else:
        FAIL += 1
        print(f"  \033[1;31mBAD \033[0m{what}   {detail}")


def call(url, method="GET", body=None, token=None):
    # The edge's `auth` bucket refills one request every three seconds with a
    # burst of four, and this probe sends seven from one address. Unpaced, the
    # sixth is nginx's 429 -- the limiter working, and a false failure here.
    if url == EDGE:
        time.sleep(3.5)
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(url, data=data, method=method)
    req.add_header("content-type", "application/json")
    if token:
        req.add_header("token", token)
    try:
        with urllib.request.urlopen(req, timeout=25) as r:
            raw, code = r.read().decode(), r.status
    except urllib.error.HTTPError as e:
        raw, code = e.read().decode(), e.code
    except Exception as exc:
        return 0, str(exc), None
    try:
        return code, raw, json.loads(raw)
    except Exception:
        return code, raw, None


def pg(sql):
    out = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres", "-d", "atlas_dev",
         "-At", "-c", sql], capture_output=True, text=True, timeout=60)
    return out.stdout.strip()


def last_completed_booking(number):
    return pg(f"""SELECT b.id FROM atlas_app.booking b
                    JOIN atlas_app.person p ON p.id = b.rider_id
                    JOIN atlas_app.ride r ON r.booking_id = b.id
                   WHERE p.unencrypted_mobile_number = '{number}' AND r.status = 'COMPLETED'
                   ORDER BY r.created_at DESC LIMIT 1""")


print("=" * 72)
print("SIGNALER — a passenger reports his driver, through the public edge")
print("=" * 72)

print("\n--- without a session")
code, raw, _ = call(EDGE, "POST", {"bookingId": "x", "text": "y"})
check(code == 401 and "missing_token" in raw, "no token is 401 missing_token", f"{code} {raw[:100]}")
code, raw, _ = call(EDGE, "POST", {"bookingId": "x", "text": "y"}, token="not-a-token")
check(code == 401 and "invalid_token" in raw, "a made-up token is 401 invalid_token", f"{code} {raw[:100]}")

print("\n--- signing a passenger in")
code, raw, a = call(f"{RIDER}/v2/auth", "POST",
                    {"mobileNumber": NUM, "mobileCountryCode": "+213", "merchantId": MERCHANT})
if not a or "authId" not in a:
    sys.exit(f"auth failed {code}: {raw[:200]}")
code, raw, v = call(f"{RIDER}/v2/auth/{a['authId']}/verify", "POST",
                    {"otp": OTP, "deviceToken": "report-probe"})
TOKEN = (v or {}).get("token")
if not TOKEN:
    sys.exit(f"verify failed {code}: {raw[:200]}")
person = pg(f"SELECT id FROM atlas_app.person WHERE unencrypted_mobile_number = '{NUM}' LIMIT 1")
mine, theirs = last_completed_booking(NUM), last_completed_booking(OTHER)
print(f"  ok — subject {person}, his booking {mine}")

print("\n--- refused bodies")
code, raw, _ = call(EDGE, "POST", {"bookingId": mine, "text": "   "}, token=TOKEN)
check(code == 400, "blank text is 400", f"{code} {raw[:100]}")
code, raw, _ = call(EDGE, "POST", {"bookingId": mine, "text": "a" * 1001}, token=TOKEN)
check(code == 400, "1001 characters is 400", f"{code} {raw[:100]}")

print("\n--- somebody else's ride")
code, raw, _ = call(EDGE, "POST", {"bookingId": theirs, "text": "probe"}, token=TOKEN)
check(code == 404 and "no_such_ride" in raw,
      "another passenger's booking is 404 no_such_ride", f"{code} {raw[:100]}")

print("\n--- his own ride")
text = "Probe : le chauffeur roulait trop vite. Ceci est un test automatique."
code, raw, r = call(EDGE, "POST", {"bookingId": mine, "text": text}, token=TOKEN)
check(code == 200 and (r or {}).get("ok") is True, "accepted with 200", f"{code} {raw[:100]}")
rid = (r or {}).get("id", "0")
row = pg(f"""SELECT rr.rider_id = '{person}',
                    rr.driver_id = d.driver_id,
                    rr.body = $${text}$$,
                    rr.status,
                    p.merchant_id
               FROM movin.ride_report rr
               JOIN atlas_app.ride ar ON ar.id = rr.ride_id
               JOIN atlas_driver_offer_bpp.ride d ON d.id = ar.bpp_ride_id
               LEFT JOIN atlas_driver_offer_bpp.person p ON p.id = rr.driver_id
              WHERE rr.id = {int(rid)}""").split("|")
check(len(row) == 5 and row[0] == "t", "stored under the token's passenger", str(row))
check(len(row) == 5 and row[1] == "t", "the driver is the one who drove that ride", str(row))
check(len(row) == 5 and row[2] == "t", "the words arrive exactly as sent", str(row))
check(len(row) == 5 and row[3] == "open", "and it waits in the queue as `open`", str(row))
if len(row) == 5:
    print(f"  (driver merchant: {row[4]} — the console lists it under that country)")

print("\n--- cleaning up")
n = pg(f"WITH d AS (DELETE FROM movin.ride_report WHERE rider_id = '{person}' "
       f"AND body LIKE 'Probe :%' RETURNING 1) SELECT count(*) FROM d")
print(f"  {n} probe report(s) removed")

print(f"\n{PASS} passed, {FAIL} failed")
sys.exit(1 if FAIL else 0)
