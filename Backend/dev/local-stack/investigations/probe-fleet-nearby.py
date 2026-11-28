#!/usr/bin/env python3
"""Prove /fleet/nearby: refused without a token, real cars with one.

    ssh ny 'cd /opt/ny/local-stack && python3 probe-fleet-nearby.py'

Read-only apart from signing a probe rider in, which the other probes do too.

Why this endpoint exists at all is in maps-shim/fleet.js. What this checks is
the part that would be embarrassing to get wrong: that it refuses a caller who
is not a signed-in passenger, and that what it returns for one who is really is
the fleet rather than a plausible-looking shape.
"""
import json
import urllib.error
import urllib.request

SHIM = "http://127.0.0.1:8030"
RIDER = "http://127.0.0.1:8014"
RIDER_MERCHANT = "YATRI"
OTP = "7891"
RIDER_NUM = "0555000199"

# Belcourt, the pickup every other probe in this directory uses.
LAT, LON = 36.7538, 3.0588


def call(method, url, body=None, token=None):
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(url, data=data, method=method)
    req.add_header("Content-Type", "application/json")
    if token:
        req.add_header("token", token)
    try:
        with urllib.request.urlopen(req, timeout=15) as r:
            return r.status, json.loads(r.read() or b"{}")
    except urllib.error.HTTPError as e:
        raw = e.read()
        try:
            return e.code, json.loads(raw or b"{}")
        except ValueError:
            return e.code, {"raw": raw[:200].decode(errors="replace")}


def main():
    print("=== 1. no token at all")
    code, _ = call("GET", f"{SHIM}/fleet/nearby?lat={LAT}&lon={LON}")
    print(f"    {code}   {'OK — refused' if code == 401 else 'WRONG — must be 401'}")

    print("\n=== 2. a token that is not a token")
    code, _ = call("GET", f"{SHIM}/fleet/nearby?lat={LAT}&lon={LON}", token="not-a-token")
    print(f"    {code}   {'OK — refused' if code == 401 else 'WRONG — must be 401'}")

    print("\n=== 3. sign a passenger in")
    _, a = call("POST", f"{RIDER}/v2/auth", {
        "mobileNumber": RIDER_NUM,
        "mobileCountryCode": "+213",
        "merchantId": RIDER_MERCHANT,
    })
    _, v = call("POST", f"{RIDER}/v2/auth/{a['authId']}/verify",
                {"otp": OTP, "deviceToken": "probe-fleet"})
    token = v["token"]
    print(f"    signed in as {RIDER_NUM}")

    print("\n=== 4. every nearby driver")
    code, body = call("GET", f"{SHIM}/fleet/nearby?lat={LAT}&lon={LON}", token=token)
    drivers = body.get("drivers", [])
    print(f"    {code}   {len(drivers)} drivers")
    for d in drivers:
        car = " ".join(x for x in [d.get("make"), d.get("model")] if x) or "(no model)"
        rating = "—" if d["rating"] is None else f"{d['rating']:.1f}"
        print(f"      {d['name'] or '(no name)':<10} {car:<22} {d['colour'] or '':<7}"
              f" {d['variant']:<14} ★{rating:<4} {d['metres']:>6} m")

    print("\n=== 5. one type only, the way the app asks")
    for variant in ("SEDAN", "AUTO_RICKSHAW", "SUV", "HATCHBACK"):
        _, body = call("GET", f"{SHIM}/fleet/nearby?lat={LAT}&lon={LON}&variant={variant}",
                       token=token)
        n = len(body.get("drivers", []))
        print(f"    {variant:<14} {n}")

    print("\n=== 6. the plate must never come back")
    leaked = [d for d in drivers if any("plate" in k or "registration" in k for k in d)]
    print(f"    {'OK — no plate in any row' if not leaked else 'LEAK: ' + str(leaked[0])}")


if __name__ == "__main__":
    main()
