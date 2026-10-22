#!/usr/bin/env python3
"""Three unused routes that decide what the remaining passenger screens should be.

Follows probe-unused-routes.py: that one says what the binary *has*, this one
says whether those routes actually work well enough to build a screen on.

Unlike the other probes this one goes through the PUBLIC edge, so it also proves
the routes survive nginx and the auth-guard.

MEASURED 2026-08-16:

  1. /v2/frontend/flowStatus -> 200 {"currentStatus":{"status":"IDLE"},
     "oldStatus":null} in 0.27s. It works. An earlier apparent timeout was a
     local connection blip, not the server -- worth saying, because that blip
     nearly got the whole recovery feature written off as impossible.

     This is the fix for the worst hole in the app: close it mid-ride and the
     ride is gone. That happened to a real tester, and the ride had to be
     cancelled from the server by hand.

  2. /v2/savedLocation -> POST 200 {"result":"Success"}, and the list returns the
     entry. BUT every address field comes back **null** -- only `tag`, `lat` and
     `lon` survive the round trip. So Home/Work is storable, but the app must
     keep its own label or re-geocode the point on the way out. Do not design a
     screen that trusts the server to hand back "Alger Centre".

  3. /v2/serviceability/destination -> Alger true, Tamanrasset true, Paris false.
     The boundary is the national border, so `true` means "inside Algeria", NOT
     "a car will come" -- Tamanrasset is 1,900 km from any driver we have. Worth
     calling before the price screen anyway, to catch a destination abroad.
"""
import json
import urllib.error
import urllib.request

BASE = "https://api.169-58-139-65.sslip.io"


def call(method, path, body=None, tok=None):
    data = json.dumps(body).encode() if body is not None else None
    q = urllib.request.Request(BASE + path, data=data, method=method)
    q.add_header("content-type", "application/json")
    if tok:
        q.add_header("token", tok)
    try:
        with urllib.request.urlopen(q, timeout=25) as r:
            return r.read().decode(), r.status
    except urllib.error.HTTPError as e:
        return e.read().decode(), e.code
    except Exception as e:
        return str(e), 0


raw, _ = call("POST", "/v2/auth", {"mobileNumber": "0555000199",
              "mobileCountryCode": "+213", "merchantId": "YATRI"})
aid = json.loads(raw)["authId"]
raw, _ = call("POST", f"/v2/auth/{aid}/verify", {"otp": "7891", "deviceToken": "extras"})
TOK = json.loads(raw)["token"]
print("auth ok\n")

print("=== 1. flowStatus — does the server track where the rider is?")
raw, code = call("GET", "/v2/frontend/flowStatus", None, TOK)
print(f"  -> {code}  {raw[:300]}")

print("\n=== 2. savedLocation — Home and Work, server-side")
raw, code = call("GET", "/v2/savedLocation/list", None, TOK)
print(f"  list -> {code}  {raw[:200]}")

save = {
    "tag": "Maison",
    "lat": 36.7538,
    "lon": 3.0588,
    "address": {"area": "Alger Centre", "city": "Alger"},
}
raw, code = call("POST", "/v2/savedLocation", save, TOK)
print(f"  save -> {code}  {raw[:220]}")

raw, code = call("GET", "/v2/savedLocation/list", None, TOK)
print(f"  list -> {code}  {raw[:300]}")

print("\n=== 3. serviceability of the DESTINATION (we only ever check origin)")
for label, gps in (("Alger", {"lat": 36.7050, "lon": 3.1750}),
                   ("Tamanrasset", {"lat": 22.7850, "lon": 5.5228}),
                   ("Paris", {"lat": 48.8566, "lon": 2.3522})):
    raw, code = call("POST", "/v2/serviceability/destination", {"location": gps}, TOK)
    print(f"  {label:<12} -> {code}  {raw[:120]}")

print("\n=== 4. does an active ride show up anywhere on a fresh start?")
raw, code = call("GET", "/v2/rideBooking/list?onlyActive=true&limit=3", None, TOK)
print(f"  onlyActive -> {code}  {raw[:240]}")
