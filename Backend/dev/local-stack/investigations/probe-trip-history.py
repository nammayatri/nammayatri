#!/usr/bin/env python3
"""What can a trip history actually show? And the mistake that nearly shipped.

Screen 15 lists the rider's past trips. The questions worth measuring are not
"does the list work" but the ones that decide what a row can say: what order it
arrives in, whether cancelled trips are mixed in, whether a rating already given
comes back, and above all **whether the two ends have names**.

── The correction this file exists to carry ────────────────────────────────
An earlier version of this probe concluded that **the backend discards every
address field** -- `fromLocation` and `toLocation` came back with `area`, `city`
and `street` all `null`. That finding was written into two screen designs, the
README, and the app, which grew a whole local label store to work around it.

It was wrong. The probe was sending `"address": {}` -- empty. Servant stored
exactly what it was given, which was nothing.

Send a real address and the booking keeps it. Both of the app's own searches
already do, so trips taken through the app name themselves; only probe-made
bookings ever looked anonymous. The lesson generalises and has now caught three
findings in this project: **absence of a value is not proof the server drops
it**, and a `200` only means the request parsed.

Note the two shapes differ, which is its own trap:
  * `rideSearch` takes a NESTED address: {"address": {...}, "gps": {...}}
  * `savedLocation` takes them FLAT: {"tag":.., "lat":.., "area":.., ...}
Sending either in the other's shape is silently ignored and answered 200.

Runs ON the VPS: rider API loopback 8013.
"""
import json
import time
import urllib.error
import urllib.request

BASE = "http://localhost:8013"
WHO = "0555009001"


def call(method, path, body=None, tok=None, tries=3):
    """Retries a timeout, never an answer -- an HTTP status is a result."""
    data = json.dumps(body).encode() if body is not None else None
    last = None
    for attempt in range(tries):
        q = urllib.request.Request(BASE + path, data=data, method=method)
        q.add_header("content-type", "application/json")
        if tok:
            q.add_header("token", tok)
        try:
            with urllib.request.urlopen(q, timeout=30) as r:
                return json.loads(r.read().decode() or "{}"), r.status
        except urllib.error.HTTPError as e:
            b = e.read().decode()
            try:
                return json.loads(b), e.code
            except Exception:
                return b[:160], e.code
        except Exception as e:
            last = e
            time.sleep(1 + attempt)
    raise SystemExit(f"timed out on {path}: {last}")


a, _ = call("POST", "/v2/auth", {"mobileNumber": WHO,
            "mobileCountryCode": "+213", "merchantId": "YATRI"})
v, _ = call("POST", f"/v2/auth/{a['authId']}/verify",
            {"otp": "7891", "deviceToken": "history"})
TOK = v["token"]
print("signed in\n")

# ── the correction, demonstrated ───────────────────────────────────────────
print("=== A. a search that NAMES both ends, the way the app does")
search, _ = call("POST", "/v2/rideSearch", {
    "fareProductType": "ONE_WAY",
    "contents": {
        "origin": {"address": {"area": "Belcourt", "city": "Alger"},
                   "gps": {"lat": 36.7538, "lon": 3.0588}},
        "destination": {"address": {"area": "Bab Ezzouar", "city": "Alger"},
                        "gps": {"lat": 36.7130, "lon": 3.1840}},
    },
}, TOK)
sid = search.get("searchId")
print(f"  search {str(sid)[:8]}")

time.sleep(4)
res, _ = call("GET", f"/v2/rideSearch/{sid}/results", None, TOK)
ests = res.get("estimates", [])
if not ests:
    raise SystemExit("no estimates — run ./setup.sh drivers, and check movin-fleet")

est = next((e for e in ests if e.get("vehicleVariant") == "SEDAN"), ests[0])
call("POST", f"/v2/estimate/{est['id']}/select", {}, TOK)

quote = None
for _ in range(30):
    time.sleep(2)
    q, _ = call("GET", f"/v2/estimate/{est['id']}/quotes", None, TOK)
    got = q.get("selectedQuotes") or []
    if got:
        quote = got[0]
        break
if not quote:
    raise SystemExit("nobody offered — is movin-fleet.service running?")

booked, _ = call("POST", f"/v2/rideSearch/quotes/{quote['id']}/confirm", {}, TOK)
bid = booked["bookingId"]
time.sleep(3)

b, _ = call("POST", f"/v2/rideBooking/{bid}", {}, TOK)
frm = b.get("fromLocation") or {}
to = (b.get("bookingDetails") or {}).get("contents", {}).get("toLocation") or {}
named = bool(frm.get("area") or frm.get("city"))

print(f"  fromLocation kept: { {k: x for k, x in frm.items() if x and k not in ('lat', 'lon')} }")
print(f"  toLocation   kept: { {k: x for k, x in to.items() if x and k not in ('lat', 'lon')} }")
print(f"  -> the booking {'KEEPS the address' if named else 'dropped it'}; "
      f"a row can name itself from the server alone: {named}")

# ── everything else a row needs ────────────────────────────────────────────
print("\n=== B. the list, as screen 15 reads it")
lst, code = call("GET", "/v2/rideBooking/list?limit=20&offset=0", None, TOK)
rows = lst.get("list", [])
print(f"  {code}, {len(rows)} trips")
for i, x in enumerate(rows[:6]):
    r = (x.get("rideList") or [{}])[0]
    print(f"   [{i}] {str(x.get('createdAt'))[:16]}  {x.get('status'):<10} "
          f"price={r.get('computedPrice')} rating={r.get('rideRating')} "
          f"variant={r.get('vehicleVariant','-')}")

stamps = [str(x.get("createdAt")) for x in rows if x.get("createdAt")]
if len(stamps) > 1:
    print(f"  order: {'NEWEST first' if stamps[0] > stamps[-1] else 'OLDEST first'}")

print("\n=== C. are cancelled trips mixed in, and do they carry a fare?")
for x in rows:
    if x.get("status") == "CANCELLED":
        r = (x.get("rideList") or [{}])[0]
        print(f"  yes — estimatedTotalFare={x.get('estimatedTotalFare')} "
              f"but computedPrice={r.get('computedPrice')}")
        print("  -> only computedPrice is money that changed hands; never print")
        print("     the estimate on a cancelled row")
        break
else:
    print("  none in this history")

print("\n=== D. does a rating already given come back?")
r = (rows[0].get("rideList") or [{}])[0] if rows else {}
print(f"  rideRating={r.get('rideRating')}  (the rider's own stars)")
print(f"  driverRatings={r.get('driverRatings')}  (the driver's average)")
print("  -> the server has NO already-rated guard and overwrites silently,")
print("     so rideRating is the only way to stop history offering twice")

print("\n=== E. filtering and paging")
for st in ("COMPLETED", "CANCELLED"):
    res2, c2 = call("GET", f"/v2/rideBooking/list?limit=5&status=%22{st}%22", None, TOK)
    print(f"  status={st:<10} -> {c2}, {len(res2.get('list', []))} rows")
tail, _ = call("GET", "/v2/rideBooking/list?limit=20&offset=20", None, TOK)
print(f"  offset=20 -> {len(tail.get('list', []))} rows "
      "(a short page is the only end-of-list signal; there is no total)")

print("\n=== F. the distance is NOT the distance driven")
r = next(((x.get("rideList") or [{}])[0] for x in rows
          if x.get("status") == "COMPLETED"), {})
print(f"  chargeableRideDistance={r.get('chargeableRideDistance')}")
print("  -> this is the distance ESTIMATED AT BOOKING. A ride cut short at 3 km")
print("     still reports 14 km. Screen 15 shows no distance for this reason.")

call("POST", f"/v2/rideBooking/{bid}/cancel",
     {"reasonCode": "CHANGE_OF_PLANS", "reasonStage": "OnAssign"}, TOK)
print("\ntest booking cancelled")
