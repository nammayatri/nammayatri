#!/usr/bin/env python3
"""The two paths screen 10 must handle that the happy run did not touch:
nobody accepts (the 300s timeout), and the rider gives up (cancel).

Deliberately does NOT bring a driver online, so the search expires naturally.
Takes ~6 minutes.
"""
import json, time, urllib.request, urllib.error
from datetime import datetime, timezone

R = "http://localhost:8013"
T0 = time.time()


def say(m):
    print(f"[{time.time()-T0:6.1f}s] {m}", flush=True)


def call(method, url, body=None, token=None):
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(url, data=data, method=method)
    req.add_header("content-type", "application/json")
    if token:
        req.add_header("token", token)
    try:
        with urllib.request.urlopen(req, timeout=25) as r:
            return r.read().decode(), r.status
    except urllib.error.HTTPError as e:
        return e.read().decode(), e.code
    except Exception as e:
        return str(e), 0


raw, _ = call("POST", f"{R}/v2/auth", {"mobileNumber": "0555000199",
              "mobileCountryCode": "+213", "merchantId": "YATRI"})
aid = json.loads(raw)["authId"]
raw, _ = call("POST", f"{R}/v2/auth/{aid}/verify", {"otp": "7891", "deviceToken": "p"})
TOK = json.loads(raw)["token"]
say("rider signed in")

# ------------------------------------------------ cancellation reason list
say("=== cancellationReason/list -- the param wants a JSON value, so quote it")
for variant in ('OnSearch', '"OnSearch"', '%22OnSearch%22'):
    raw, code = call("GET",
                     f"{R}/v2/cancellationReason/list?cancellationStage={variant}",
                     None, TOK)
    say(f"  {variant:18} -> {code} {raw[:220]}")

# --------------------------------------------------------- search that dies
say("=== a search nobody will accept (no driver is online)")
raw, _ = call("POST", f"{R}/v2/rideSearch", {
    "fareProductType": "ONE_WAY",
    "contents": {
        "origin": {"address": {"area": "Alger Centre", "city": "Alger"},
                   "gps": {"lat": 36.7538, "lon": 3.0588}},
        "destination": {"address": {"area": "Hussein Dey", "city": "Alger"},
                        "gps": {"lat": 36.7050, "lon": 3.1750}}}}, TOK)
b = json.loads(raw)
SEARCH, EXP = b["searchId"], b.get("searchExpiry")
say(f"  searchId={SEARCH} expiry={EXP}")

EST = None
for _ in range(15):
    raw, _ = call("GET", f"{R}/v2/rideSearch/{SEARCH}/results", None, TOK)
    e = json.loads(raw).get("estimates", [])
    if e:
        EST = next((x for x in e if x.get("vehicleVariant") == "SEDAN"), e[0])
        break
    time.sleep(1)
say(f"  estimate {EST['id']} ({EST['vehicleVariant']}, {EST['estimatedTotalFare']})")

call("POST", f"{R}/v2/estimate/{EST['id']}/select", None, TOK)
say("  selected -- now watching until past expiry")

for t in (30, 60, 120, 180, 240, 290, 305, 320):
    while time.time() - T0 < t:
        time.sleep(1)
    q, qc = call("GET", f"{R}/v2/estimate/{EST['id']}/quotes", None, TOK)
    r, rc = call("GET", f"{R}/v2/rideSearch/{SEARCH}/results", None, TOK)
    nest = len(json.loads(r).get("estimates", [])) if rc == 200 else -1
    say(f"  t={t:3}s  quotes -> {qc} {q[:90]}   results -> {rc} ({nest} estimates)")

say("=== after expiry: can the rider still select or confirm?")
raw, code = call("POST", f"{R}/v2/estimate/{EST['id']}/select", None, TOK)
say(f"  select again -> {code} {raw[:200]}")

# ------------------------------------------------------------ explicit cancel
say("=== a fresh search, then the rider cancels while waiting")
raw, _ = call("POST", f"{R}/v2/rideSearch", {
    "fareProductType": "ONE_WAY",
    "contents": {
        "origin": {"address": {"area": "Alger Centre", "city": "Alger"},
                   "gps": {"lat": 36.7538, "lon": 3.0588}},
        "destination": {"address": {"area": "Hussein Dey", "city": "Alger"},
                        "gps": {"lat": 36.7050, "lon": 3.1750}}}}, TOK)
S2 = json.loads(raw)["searchId"]
E2 = None
for _ in range(15):
    raw, _ = call("GET", f"{R}/v2/rideSearch/{S2}/results", None, TOK)
    e = json.loads(raw).get("estimates", [])
    if e:
        E2 = next((x for x in e if x.get("vehicleVariant") == "SEDAN"), e[0])
        break
    time.sleep(1)
call("POST", f"{R}/v2/estimate/{E2['id']}/select", None, TOK)
say(f"  selected {E2['id']}")
raw, code = call("POST", f"{R}/v2/estimate/{E2['id']}/cancel", None, TOK)
say(f"  estimate/cancel -> {code} {raw[:200]}")
raw, code = call("GET", f"{R}/v2/estimate/{E2['id']}/quotes", None, TOK)
say(f"  quotes after cancel -> {code} {raw[:200]}")
raw, code = call("GET", f"{R}/v2/rideBooking/list?limit=3&onlyActive=true", None, TOK)
say(f"  active bookings after cancel -> {code} {raw[:300]}")
say("done")
