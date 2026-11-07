#!/usr/bin/env python3
"""Does the rider's own profile carry a rating the app could show her?

The client asked (2026-08-25) for the passenger's star rating on her profile
screen. The rating a driver gives her is written by our own patch into
`atlas_driver_offer_bpp.rider_details` -- the provider side. This asks the
*rider* binary whether it would hand her anything at all, before deciding
whether to bridge the two schemas or to answer from maps-shim.

Prints the whole profile body, because the question is which field names exist,
not what today's value is.
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
raw, _ = call("POST", f"/v2/auth/{aid}/verify", {"otp": "7891", "deviceToken": "rating"})
TOK = json.loads(raw)["token"]
print("auth ok\n")

raw, code = call("GET", "/v2/profile", None, TOK)
print(f"GET /v2/profile -> {code}")
try:
    body = json.loads(raw)
    for k in sorted(body):
        print(f"   {k} = {json.dumps(body[k])[:80]}")
except Exception:
    print("   " + raw[:600])
