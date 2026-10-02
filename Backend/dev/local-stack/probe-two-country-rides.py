#!/usr/bin/env python3
"""Step 8 of the two-country plan: a whole ride in each country, and its charge.

    python3 probe-two-country-rides.py mr     # Mauritania only
    python3 probe-two-country-rides.py dz     # Algeria only (needs the +213 build)
    python3 probe-two-country-rides.py both

Runs ON the VPS: rider API on loopback 8013, driver API on loopback 8017, so the
auth guard is not in the way and the backend's fixed code (7891) applies.

── Who drives, and why it is not the same in both countries ───────────────────
MAURITANIA: nobody here signs in as a driver. The simulator daemon drives
22100001-06 and accepts whatever reaches them. Signing in as one of them from a
probe revokes the daemon's session and the car goes quiet for an hour; signing
in as 22100009 would sign the developer's phone out. So the probe is the
PASSENGER only, and the simulator is the driver -- which is also the more honest
test, since it is the path a real passenger takes.

ALGERIA: the simulator does not know the Algerian merchant. The probe signs in
as one of the pilot's parked +213 drivers (no daemon holds them) and plays that
side itself: offer, arrive, start with the passenger's code, end.

── What counts as proof ───────────────────────────────────────────────────────
  * the ride ends COMPLETED, with a price in the country's own scale
  * the wallet charged that driver's country's day: -30 (MRU) or -100 (DA)
    for the ride, by maps-shim's sweep, which runs on its own timer

Every booking it opens, it finishes -- an unfinished one locks the rider out of
confirming anything (ACTIVE_BOOKING_PRESENT).
"""
import json
import subprocess
import sys
import time
import urllib.error
import urllib.request
from datetime import datetime, timezone

R = "http://localhost:8013"
D = "http://localhost:8017"
OTP = "7891"
T0 = time.time()

COUNTRIES = {
    "mr": dict(name="Mauritania", rider=("+222", "22778899"),
               merchant="favorit0-0000-0000-0000-00000favorit",
               pickup=(18.0858, -15.9582), drop=(18.1030, -15.9500),
               currency="MRU", day=30, drive_self=False),
    "dz": dict(name="Algeria", rider=("+213", "0555000199"),
               merchant="algeria0-0000-0000-0000-00000algeria",
               pickup=(36.7538, 3.0588), drop=(36.7050, 3.1750),
               currency="DZD", day=100, drive_self=True),
}
VARIANT = "SEDAN"


def say(msg):
    print(f"[{time.time() - T0:6.1f}s] {msg}", flush=True)


def pg(sql):
    out = subprocess.run(["docker", "exec", "ny-postgres", "psql", "-U", "postgres",
                          "-d", "atlas_dev", "-At", "-c", sql],
                         capture_output=True, text=True, timeout=30)
    return out.stdout.strip()


def call(method, url, body=None, token=None):
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
    except Exception as e:
        return None, 0, str(e)
    try:
        return json.loads(raw), code, raw
    except Exception:
        return raw, code, raw


def now_iso():
    return datetime.now(timezone.utc).isoformat().replace("+00:00", "Z")


def sign_in(base, path, cc, number, merchant):
    a, code, raw = call("POST", f"{base}{path}", {
        "mobileCountryCode": cc, "mobileNumber": number, "merchantId": merchant})
    if not isinstance(a, dict) or "authId" not in a:
        say(f"  sign-in {cc} {number} -> {code} {str(raw)[:160]}")
        return None
    v, code, raw = call("POST", f"{base}{path}/{a['authId']}/verify",
                        {"otp": OTP, "deviceToken": "two-country-rides-probe"})
    tok = v.get("token") if isinstance(v, dict) else None
    if not tok:
        say(f"  verify {cc} {number} -> {code} {str(raw)[:160]}")
    return tok


def run(key):
    c = COUNTRIES[key]
    say(f"===== {c['name']} =====")
    ok = True

    # ── the driver side, when the probe plays it ──────────────────────────
    dtok = did = None
    if c["drive_self"]:
        row = pg(f"""SELECT p.unencrypted_mobile_number || '~' || p.id
                      FROM atlas_driver_offer_bpp.person p
                      JOIN atlas_driver_offer_bpp.vehicle v ON v.driver_id = p.id
                      JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
                     WHERE p.merchant_id = '{c['merchant']}' AND v.variant = '{VARIANT}'
                       AND di.enabled AND NOT di.blocked AND NOT di.on_ride
                       AND p.unencrypted_mobile_number IS NOT NULL
                     ORDER BY p.unencrypted_mobile_number LIMIT 1""")
        if not row:
            say("  no usable driver in this merchant"); return False
        dnum, did = row.split("~")
        dtok = sign_in(D, "/ui/auth", c["rider"][0], dnum, c["merchant"])
        if not dtok:
            return False
        say(f"  driver {dnum} signed in")
        # Put him AT the pickup with a fresh stamp, in the table the pool reads.
        # The API alone is not enough: a position whose `ts` is not newer than
        # the stored one is dropped with a 200, and the keep-alive timer stamps
        # every row every two minutes -- so the first run of this probe left him
        # 142 m out with a 134 s-old fix, and dispatch chose five others.
        lat, lon = c["pickup"][0] + 0.0002, c["pickup"][1] + 0.0002
        pg(f"""UPDATE atlas_driver_offer_bpp.driver_location
                  SET lat = {lat}, lon = {lon},
                      point = ST_SetSRID(ST_Point({lon}, {lat}), 4326),
                      coordinates_calculated_at = now(), updated_at = now()
                WHERE driver_id = '{did}'""")
        call("POST", f"{D}/ui/driver/setActivity?active=true", None, token=dtok)

    # ── the passenger ─────────────────────────────────────────────────────
    rtok = sign_in(R, "/v2/auth", c["rider"][0], c["rider"][1], "YATRI")
    if not rtok:
        return False
    say(f"  rider {c['rider'][0]} {c['rider'][1]} signed in")

    s, code, raw = call("POST", f"{R}/v2/rideSearch", {
        "fareProductType": "ONE_WAY",
        "contents": {
            "origin": {"address": {"area": "A", "city": "A"},
                       "gps": {"lat": c["pickup"][0], "lon": c["pickup"][1]}},
            "destination": {"address": {"area": "B", "city": "B"},
                            "gps": {"lat": c["drop"][0], "lon": c["drop"][1]}}}},
        token=rtok)
    sid = s.get("searchId") if isinstance(s, dict) else None
    if not sid:
        say(f"  search -> {code} {str(raw)[:160]}"); return False

    est = None
    for _ in range(15):
        r, _, _ = call("GET", f"{R}/v2/rideSearch/{sid}/results", token=rtok)
        ests = r.get("estimates", []) if isinstance(r, dict) else []
        est = next((e for e in ests if e.get("vehicleVariant") == VARIANT), None)
        if est:
            break
        time.sleep(1)
    if not est:
        say(f"  no {VARIANT} estimate"); return False
    say(f"  estimate {est.get('estimatedTotalFare')} {c['currency']}")
    call("POST", f"{R}/v2/estimate/{est['id']}/select", None, token=rtok)

    if dtok:
        req = None
        for _ in range(30):
            n, _, _ = call("GET", f"{D}/ui/driver/nearbyRideRequest", token=dtok)
            reqs = n.get("searchRequestsForDriver", []) if isinstance(n, dict) else []
            if reqs:
                req = reqs[0]; break
            time.sleep(1)
        if not req:
            say("  the driver never saw the request"); return False
        sreq = req.get("searchRequestId") or req.get("id")
        _, code, raw = call("POST", f"{D}/ui/driver/searchRequest/quote/respond",
                            {"searchRequestId": sreq, "response": "Accept"}, token=dtok)
        say(f"  driver accepted -> {code}")

    quote = None
    for _ in range(45):
        q, _, _ = call("GET", f"{R}/v2/estimate/{est['id']}/quotes", token=rtok)
        sq = q.get("selectedQuotes", []) if isinstance(q, dict) else []
        if sq:
            quote = sq[0]; break
        time.sleep(2)
    if not quote:
        say("  no offer reached the rider in 90 s (is the simulator running?)"); return False

    cf, code, raw = call("POST", f"{R}/v2/rideSearch/quotes/{quote['id']}/confirm", None, token=rtok)
    booking = cf.get("bookingId") if isinstance(cf, dict) else None
    if not booking:
        say(f"  confirm -> {code} {str(raw)[:200]}"); return False
    say(f"  booked {booking}")

    ride = None
    for _ in range(30):
        b, _, _ = call("POST", f"{R}/v2/rideBooking/{booking}", None, token=rtok)
        rl = b.get("rideList", []) if isinstance(b, dict) else []
        if rl:
            ride = rl[0]; break
        time.sleep(2)
    if not ride:
        say("  never assigned"); return False
    bpp_ride = ride["bppRideId"]
    say(f"  assigned: {ride.get('vehicleModel')} {ride.get('vehicleNumber')}")

    if dtok:
        call("POST", f"{D}/ui/driver/ride/{bpp_ride}/arrived/pickup",
             {"lat": c["pickup"][0], "lon": c["pickup"][1]}, token=dtok)
        _, code, raw = call("POST", f"{D}/ui/driver/ride/{bpp_ride}/start",
                            {"rideOtp": ride.get("rideOtp"),
                             "point": {"lat": c["pickup"][0], "lon": c["pickup"][1]}}, token=dtok)
        say(f"  start -> {code}")
        _, code, raw = call("POST", f"{D}/ui/driver/ride/{bpp_ride}/end",
                            {"point": {"lat": c["drop"][0], "lon": c["drop"][1]}}, token=dtok)
        say(f"  end -> {code}")

    final = {}
    for _ in range(150):      # the simulator drives at 8x; allow 5 minutes
        b, _, _ = call("POST", f"{R}/v2/rideBooking/{booking}", None, token=rtok)
        final = (b.get("rideList") or [{}])[0] if isinstance(b, dict) else {}
        if final.get("status") in ("COMPLETED", "CANCELLED"):
            break
        time.sleep(2)
    say(f"  ride {final.get('status')}  price {final.get('computedPrice')} {c['currency']}"
        f"  distance {final.get('chargeableRideDistance')} m")
    ok = ok and final.get("status") == "COMPLETED"
    call("POST", f"{R}/v2/feedback/rateRide",
         {"rideId": ride["id"], "rating": 5, "feedbackDetails": "two-country probe"}, token=rtok)

    # ── the day charged, in the driver's own country ──────────────────────
    charged = ""
    for _ in range(40):       # the sweep runs on maps-shim's own timer
        charged = pg(f"SELECT amount FROM movin.wallet_entry WHERE ride_id = '{bpp_ride}'")
        if charged:
            break
        time.sleep(5)
    covered_note = " (0 = the driver's day was already paid)" if charged == "0" else ""
    good = charged in (str(-c["day"]), "0")
    say(f"  wallet entry for this ride: {charged or 'none after 200 s'}{covered_note}"
        f"  -> {'ok' if good else 'WRONG'} (expected -{c['day']} {c['currency']})")
    ok = ok and good

    if dtok:
        # Back into the parked fleet, as it was.
        call("POST", f"{D}/ui/driver/setActivity?active=true", None, token=dtok)
    return ok


which = (sys.argv[1] if len(sys.argv) > 1 else "both").lower()
keys = ["mr", "dz"] if which == "both" else [which]
results = {k: run(k) for k in keys}
say("RESULT  " + "  ".join(f"{COUNTRIES[k]['name']}={'PASS' if v else 'FAIL'}" for k, v in results.items()))
sys.exit(0 if all(results.values()) else 1)
