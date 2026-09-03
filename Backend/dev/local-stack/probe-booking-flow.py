#!/usr/bin/env python3
"""Drive a whole ride from BOTH sides and record what the rider API actually says.

Step 0 of the passenger-app plan. openapi gives the shapes; it cannot say what
"no driver has offered yet" looks like, how long a search really lives, or
whether the rider's tracking endpoint returns a point that moves. Those decide
screens 10-13, so they get measured.

Runs ON the VPS: rider API is loopback 8013, driver API loopback 8017.
Neither is reachable from outside, which is why this is not a laptop script.

Two traps this script is written around, both already paid for once:
  * driver `merchantId` is the merchant UUID; the rider side wants the short id
  * a driver position whose `ts` is not newer than the stored one is DROPPED
    and the server still answers 200
"""
import json
import subprocess
import sys
import time
import urllib.error
import urllib.request
from datetime import datetime, timedelta, timezone

R = "http://localhost:8013"
D = "http://localhost:8017"
RIDER_MERCHANT = "YATRI"
OTP = "7891"

T0 = time.time()
LOG = []


def el():
    return f"[{time.time() - T0:6.1f}s]"


def say(msg):
    print(f"{el()} {msg}", flush=True)


def pg(sql, db="atlas_dev"):
    # No `docker exec -i`: the -i steals the stdin this script arrives on.
    out = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres", "-d", db,
         "-At", "-c", sql],
        capture_output=True, text=True, timeout=30)
    return out.stdout.strip()


def call(method, url, body=None, token=None, quiet=False):
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(url, data=data, method=method)
    req.add_header("content-type", "application/json")
    if token:
        req.add_header("token", token)
    try:
        with urllib.request.urlopen(req, timeout=25) as r:
            raw = r.read().decode()
            code = r.status
    except urllib.error.HTTPError as e:
        raw, code = e.read().decode(), e.code
    except Exception as e:
        return None, 0, str(e)
    try:
        parsed = json.loads(raw)
    except Exception:
        parsed = raw
    if not quiet:
        say(f"  {method} {url.split('localhost:')[1][5:]} -> {code}")
    return parsed, code, raw


def show(label, obj, n=600):
    s = json.dumps(obj, indent=2, default=str) if not isinstance(obj, str) else obj
    if len(s) > n:
        s = s[:n] + f"\n  ... (+{len(s)-n} chars)"
    print(f"\n----- {label}\n{s}\n", flush=True)


# --------------------------------------------------------------- rider sign-in
say("=== RIDER: sign in")
RIDER_NUM = "0555000199"
a, _, raw = call("POST", f"{R}/v2/auth", {
    "mobileNumber": RIDER_NUM, "mobileCountryCode": "+213",
    "merchantId": RIDER_MERCHANT})
if not isinstance(a, dict) or "authId" not in a:
    show("rider auth FAILED", raw)
    sys.exit(1)
v, _, raw = call("POST", f"{R}/v2/auth/{a['authId']}/verify",
                 {"otp": OTP, "deviceToken": "probe-rider"})
RTOK = v.get("token") if isinstance(v, dict) else None
if not RTOK:
    show("rider verify FAILED", raw)
    sys.exit(1)
say(f"  rider token ok, person={v.get('person',{}).get('id','?')}")

# -------------------------------------------------------------- driver sign-in
say("=== DRIVER: sign in")
# Pin cc='+213'. The other seeded drivers are upstream's Indian and Sri Lankan
# ones: LIMIT 1 picked a +91 number whose country code the validator rejects.
cred = pg("""SELECT p.unencrypted_mobile_number || '~' || p.mobile_country_code
                    || '~' || m.id || '~' || p.id || '~' || coalesce(v.variant,'NONE')
               FROM atlas_driver_offer_bpp.person p
               JOIN atlas_driver_offer_bpp.merchant m ON m.id = p.merchant_id
               LEFT JOIN atlas_driver_offer_bpp.vehicle v ON v.driver_id = p.id
              WHERE p.role='DRIVER' AND p.unencrypted_mobile_number IS NOT NULL
                AND p.mobile_country_code = '+213'
              LIMIT 1;""")
if not cred:
    say("  no Algerian driver has a readable number -- cannot drive the BPP side")
    sys.exit(1)
DNUM, DCC, DMER, DID, DVARIANT = cred.split("~")
say(f"  driver {DNUM} cc={DCC} id={DID} variant={DVARIANT}")

HOME = pg(f"SELECT lat || ',' || lon FROM atlas_driver_offer_bpp.driver_location "
          f"WHERE driver_id='{DID}';")
say(f"  position before this probe: {HOME or 'NONE'}  (restored at the end)")

a, _, raw = call("POST", f"{D}/ui/auth", {
    "mobileNumber": DNUM, "mobileCountryCode": DCC, "merchantId": DMER})
if not isinstance(a, dict) or "authId" not in a:
    show("driver auth FAILED", raw)
    sys.exit(1)
v, _, raw = call("POST", f"{D}/ui/auth/{a['authId']}/verify",
                 {"otp": OTP, "deviceToken": "probe-driver"})
DTOK = v.get("token") if isinstance(v, dict) else None
if not DTOK:
    show("driver verify FAILED", raw)
    sys.exit(1)
say("  driver token ok")

# Put the driver near the pickup and mark him online, or nothing will be offered.
PICKUP = (36.7538, 3.0588)      # central Algiers
DROP = (36.7050, 3.1750)        # ~13 km east
now = datetime.now(timezone.utc)
call("POST", f"{D}/ui/driver/location",
     [{"pt": {"lat": PICKUP[0] + 0.002, "lon": PICKUP[1] + 0.002},
       "ts": now.isoformat().replace("+00:00", "Z"), "acc": 8.0}], token=DTOK)
call("POST", f"{D}/ui/driver/setActivity?active=true", None, token=DTOK)

# ------------------------------------------------------------------ the search
say("=== RIDER: search")
s, _, raw = call("POST", f"{R}/v2/rideSearch", {
    "fareProductType": "ONE_WAY",
    "contents": {
        "origin": {"address": {"area": "Alger Centre", "city": "Alger"},
                   "gps": {"lat": PICKUP[0], "lon": PICKUP[1]}},
        "destination": {"address": {"area": "Hussein Dey", "city": "Alger"},
                        "gps": {"lat": DROP[0], "lon": DROP[1]}}}},
    token=RTOK)
SEARCH = s.get("searchId") if isinstance(s, dict) else None
EXPIRY = s.get("searchExpiry") if isinstance(s, dict) else None
say(f"  searchId={SEARCH}")
say(f"  searchExpiry={EXPIRY}")
if EXPIRY:
    try:
        exp = datetime.fromisoformat(EXPIRY.replace("Z", "+00:00"))
        say(f"  -> search lives {(exp - datetime.now(timezone.utc)).total_seconds():.0f}s"
            f"  (screen 10's real timeout)")
    except Exception:
        pass

say("  polling /results until estimates appear")
EST = None
for i in range(15):
    r, _, _ = call("GET", f"{R}/v2/rideSearch/{SEARCH}/results", token=RTOK, quiet=True)
    ests = r.get("estimates", []) if isinstance(r, dict) else []
    if ests:
        say(f"  {len(ests)} estimates after {i+1} polls")
        say("  variants offered: " + ", ".join(
            f"{e.get('vehicleVariant')}={e.get('estimatedTotalFare')}" for e in ests))
        # Dispatch only offers a search to drivers whose vehicle variant matches
        # the estimate the rider picked. Taking estimates[0] blindly sent the
        # last run to two AUTO_RICKSHAW seeds and our SEDAN never saw it.
        EST = next((e for e in ests if e.get("vehicleVariant") == DVARIANT), None)
        if EST is None:
            say(f"  NO estimate matches our driver's {DVARIANT} -- "
                f"a rider picking any other row would wait 300s for nobody")
            sys.exit(1)
        say(f"  picking the {DVARIANT} estimate, the one our driver can serve")
        show("that estimate", EST, 700)
        break
    time.sleep(1)
if not EST:
    say("  NO ESTIMATES -- stopping"); sys.exit(1)
EID = EST["id"]

# ------------------------------------------------- what 'no driver yet' returns
say("=== RIDER: quotes BEFORE anyone offers  (this is screen 10's empty state)")
q, code, raw = call("GET", f"{R}/v2/estimate/{EID}/quotes", token=RTOK)
show(f"quotes before select (HTTP {code})", raw, 400)

say("=== RIDER: select  -> publishes the request to drivers")
call("POST", f"{R}/v2/estimate/{EID}/select", None, token=RTOK)
q, code, raw = call("GET", f"{R}/v2/estimate/{EID}/quotes", token=RTOK)
show(f"quotes right after select (HTTP {code})", raw, 400)

# ------------------------------------------------------------ the driver offers
say("=== DRIVER: look for the request and offer a fare")
SREQ = None
for i in range(20):
    n, _, raw = call("GET", f"{D}/ui/driver/nearbyRideRequest", token=DTOK, quiet=True)
    reqs = n.get("searchRequestsForDriver", []) if isinstance(n, dict) else []
    if reqs:
        SREQ = reqs[0]
        say(f"  driver sees the request after {i+1} polls")
        show("what the DRIVER sees", SREQ, 700)
        break
    time.sleep(1)

if SREQ:
    sid = SREQ.get("searchRequestId") or SREQ.get("id")
    # `offeredFare` is NOT the total. Sending baseFare (852) was rejected with
    # EXTRA_FEE_NOT_ALLOWED, so it is the EXTRA the driver adds on top, bounded
    # by driverMinExtraFee..driverMaxExtraFee. Omitting it accepts at base fare,
    # which is what a driver tapping "accept" does.
    say(f"  extra fee allowed: {SREQ.get('driverMinExtraFee')}"
        f"..{SREQ.get('driverMaxExtraFee')} on top of baseFare {SREQ.get('baseFare')}")
    attempts = [
        ("respond Accept, no extra", "respond",
         {"searchRequestId": sid, "response": "Accept"}),
        ("respond Accept, extra=50", "respond",
         {"searchRequestId": sid, "response": "Accept", "offeredFare": 50}),
        ("offer extra=50", "offer",
         {"searchRequestId": sid, "offeredFare": 50}),
    ]
    for desc, ep, body in attempts:
        o, code, raw = call("POST", f"{D}/ui/driver/searchRequest/quote/{ep}",
                            body, token=DTOK)
        say(f"  {desc:26} -> {code} {str(raw)[:140]}")
        if code == 200:
            say(f"  ACCEPTED VIA: /{ep} {json.dumps(body)}")
            break
else:
    say("  driver never saw the request")

# ------------------------------------------------- the quote arrives for rider
say("=== RIDER: poll quotes until the offer lands  (screen 10's happy path)")
QUOTE = None
t_sel = time.time()
for i in range(25):
    q, _, raw = call("GET", f"{R}/v2/estimate/{EID}/quotes", token=RTOK, quiet=True)
    sq = q.get("selectedQuotes", []) if isinstance(q, dict) else []
    if sq:
        say(f"  offer visible to rider after {time.time()-t_sel:.1f}s ({i+1} polls)")
        QUOTE = sq[0]
        show("the quote the rider sees", QUOTE, 800)
        break
    time.sleep(2)
if not QUOTE:
    say("  no quote reached the rider -- stopping before confirm")
    pg(f"UPDATE atlas_driver_offer_bpp.driver_location SET lat={HOME.split(',')[0]},"
       f" lon={HOME.split(',')[1]} WHERE driver_id='{DID}';" if HOME else "SELECT 1;")
    sys.exit(1)

# ------------------------------------------------------------------- confirm
say("=== RIDER: confirm the quote")
c, code, raw = call("POST", f"{R}/v2/rideSearch/quotes/{QUOTE['id']}/confirm",
                    None, token=RTOK)
BOOKING = c.get("bookingId") if isinstance(c, dict) else None
say(f"  bookingId={BOOKING}")
if not BOOKING:
    show("confirm FAILED", raw); sys.exit(1)

say("=== RIDER: poll the booking until a driver is assigned")
RIDE = None
OTP_RIDE = None
for i in range(20):
    b, _, raw = call("POST", f"{R}/v2/rideBooking/{BOOKING}", None, token=RTOK, quiet=True)
    st = b.get("status") if isinstance(b, dict) else None
    rl = b.get("rideList", []) if isinstance(b, dict) else []
    if i == 0:
        say(f"  first poll: status={st} rides={len(rl)}")
    if rl:
        RIDE = rl[0]
        OTP_RIDE = RIDE.get("rideOtp")
        say(f"  assigned after {i+1} polls: status={st} rideStatus={RIDE.get('status')}")
        show("what screen 11 gets", {k: RIDE.get(k) for k in
             ("id", "status", "driverName", "driverNumber", "driverRatings",
              "vehicleNumber", "vehicleColor", "vehicleModel", "vehicleVariant",
              "rideOtp", "shortRideId", "driverArrivalTime")}, 800)
        break
    time.sleep(2)
if not RIDE:
    say("  never assigned"); sys.exit(1)
RID = RIDE["id"]
BPP_RIDE = RIDE["bppRideId"]

# ------------------------------------------------------------------- tracking
say("=== RIDER: does the tracking endpoint return a point that MOVES?")
call("POST", f"{D}/ui/driver/location",
     [{"pt": {"lat": PICKUP[0] + 0.004, "lon": PICKUP[1] + 0.004},
       "ts": datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
       "acc": 8.0}], token=DTOK)
time.sleep(2)
p1, code, raw = call("POST", f"{R}/v2/ride/{RID}/driver/location", None, token=RTOK)
say(f"  first read  -> {code} {raw}")
call("POST", f"{D}/ui/driver/location",
     [{"pt": {"lat": PICKUP[0] + 0.010, "lon": PICKUP[1] + 0.010},
       "ts": (datetime.now(timezone.utc) + timedelta(seconds=3))
             .isoformat().replace("+00:00", "Z"), "acc": 8.0}], token=DTOK)
time.sleep(3)
p2, code, raw = call("POST", f"{R}/v2/ride/{RID}/driver/location", None, token=RTOK)
say(f"  second read -> {code} {raw}")
if isinstance(p1, dict) and isinstance(p2, dict):
    say("  MOVED" if p1 != p2 else "  DID NOT MOVE -- tracking would look frozen")

# ---------------------------------------------------------- cancellation menu
say("=== RIDER: cancellation reasons at each stage")
for stage in ("OnSearch", "OnConfirm", "OnAssign"):
    cr, code, raw = call("GET", f"{R}/v2/cancellationReason/list?cancellationStage={stage}",
                         token=RTOK, quiet=True)
    n = len(cr) if isinstance(cr, list) else 0
    say(f"  {stage}: {n} reasons  {str(raw)[:180]}")

# ------------------------------------------------------------- run the ride
say("=== DRIVER: arrive, start, end")
_, code, raw = call("POST", f"{D}/ui/driver/ride/{BPP_RIDE}/arrived/pickup",
                    {"lat": PICKUP[0], "lon": PICKUP[1]}, token=DTOK)
say(f"  arrived -> {code} {str(raw)[:100]}")
b, _, _ = call("POST", f"{R}/v2/rideBooking/{BOOKING}", None, token=RTOK, quiet=True)
say(f"  rider now sees driverArrivalTime="
    f"{(b.get('rideList') or [{}])[0].get('driverArrivalTime')}")

_, code, raw = call("POST", f"{D}/ui/driver/ride/{BPP_RIDE}/start",
                    {"rideOtp": OTP_RIDE, "point": {"lat": PICKUP[0], "lon": PICKUP[1]}},
                    token=DTOK)
say(f"  start (otp {OTP_RIDE}) -> {code} {str(raw)[:100]}")
b, _, _ = call("POST", f"{R}/v2/rideBooking/{BOOKING}", None, token=RTOK, quiet=True)
say(f"  rider sees rideStatus={(b.get('rideList') or [{}])[0].get('status')}"
    f" bookingStatus={b.get('status')}")

_, code, raw = call("POST", f"{D}/ui/driver/ride/{BPP_RIDE}/end",
                    {"point": {"lat": DROP[0], "lon": DROP[1]}}, token=DTOK)
say(f"  end -> {code} {str(raw)[:100]}")
time.sleep(2)
b, _, raw = call("POST", f"{R}/v2/rideBooking/{BOOKING}", None, token=RTOK, quiet=True)
fin = (b.get("rideList") or [{}])[0] if isinstance(b, dict) else {}
say(f"  FINAL bookingStatus={b.get('status')} rideStatus={fin.get('status')}")
show("what screen 13 gets", {
    "estimatedTotalFare": b.get("estimatedTotalFare"),
    "computedPrice": fin.get("computedPrice"),
    "chargeableRideDistance": fin.get("chargeableRideDistance"),
    "rideStartTime": fin.get("rideStartTime"),
    "rideEndTime": fin.get("rideEndTime"),
    "fareBreakup": b.get("fareBreakup"),
}, 700)

say("=== RIDER: rate the ride")
_, code, raw = call("POST", f"{R}/v2/feedback/rateRide",
                    {"rideId": RID, "rating": 5, "feedbackDetails": "probe"},
                    token=RTOK)
say(f"  rateRide -> {code} {str(raw)[:100]}")

# ---------------------------------------------------------------- put it back
say("=== cleanup: driver offline, position restored")
call("POST", f"{D}/ui/driver/setActivity?active=false", None, token=DTOK, quiet=True)
if HOME:
    lat, lon = HOME.split(",")
    pg(f"UPDATE atlas_driver_offer_bpp.driver_location "
       f"SET lat={lat}, lon={lon} WHERE driver_id='{DID}';")
    say(f"  restored to {HOME}")
say("done")
