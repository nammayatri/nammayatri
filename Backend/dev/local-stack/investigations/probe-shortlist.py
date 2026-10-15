#!/usr/bin/env python3
"""Prove the passenger's shortlist actually filters who gets asked.

Two searches, back to back, on the same pickup and the same vehicle type:

    A. select2 with no shortlist   -> every eligible driver is asked
    B. select2 with ONE driver id  -> exactly that driver is asked

`search_request_for_driver` on the provider side is the record of who was
actually asked, so this reads the answer out of the database rather than
inferring it from what the app displays.

The two are run in this order deliberately: A establishes how many drivers the
pool would have found, so B proving "1" means something. If A also returned 1
the test proves nothing, and it says so.

Creates two real search requests. They expire on their own; nothing is booked.
"""
import json
import subprocess
import sys
import time
import urllib.error
import urllib.request

RIDER = "http://127.0.0.1:8013"
SHIM = "http://127.0.0.1:8030"
NUM = "0555000199"
OTP = "7891"
LAT, LON = 36.7538, 3.0588
VARIANT = "SEDAN"


def call(method, url, body=None, token=None):
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(url, data=data, method=method)
    req.add_header("Content-Type", "application/json")
    if token:
        req.add_header("token", token)
    try:
        with urllib.request.urlopen(req, timeout=20) as r:
            return r.status, json.loads(r.read() or b"{}")
    except urllib.error.HTTPError as e:
        raw = e.read()
        try:
            return e.code, json.loads(raw or b"{}")
        except ValueError:
            return e.code, {"raw": raw[:200].decode(errors="replace")}


def sql(query):
    out = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres", "-d", "atlas_dev",
         "-t", "-A", "-F", "|", "-c", query],
        capture_output=True, text=True,
    )
    if out.returncode != 0:
        print("   SQL failed:", out.stderr.strip()[:200])
        return []
    return [l for l in out.stdout.strip().splitlines() if l]


def sign_in():
    _, a = call("POST", f"{RIDER}/v2/auth",
                {"mobileNumber": NUM, "mobileCountryCode": "+213", "merchantId": "YATRI"})
    _, v = call("POST", f"{RIDER}/v2/auth/{a['authId']}/verify",
                {"otp": OTP, "deviceToken": "probe-shortlist"})
    return v["token"]


def search(token):
    _, s = call("POST", f"{RIDER}/v2/rideSearch", {
        "fareProductType": "ONE_WAY",
        "contents": {
            "origin": {"gps": {"lat": LAT, "lon": LON},
                       "address": {"area": "Belcourt", "city": "Alger",
                                   "country": "Algeria", "state": "Alger"}},
            "destination": {"gps": {"lat": 36.77, "lon": 3.06},
                            "address": {"area": "Alger Centre", "city": "Alger",
                                        "country": "Algeria", "state": "Alger"}},
        }}, token=token)
    sid = s.get("searchId")
    time.sleep(6)
    _, r = call("GET", f"{RIDER}/v2/rideSearch/{sid}/results", token=token)
    est = next((e for e in r.get("estimates", []) if e.get("vehicleVariant") == VARIANT), None)
    return sid, est


def asked(transaction_id):
    """Which drivers the provider actually sent this search to."""
    rows = sql(
        "SELECT srfd.driver_id, p.first_name "
        "  FROM atlas_driver_offer_bpp.search_request_for_driver srfd "
        "  JOIN atlas_driver_offer_bpp.search_request sr ON sr.id = srfd.search_request_id "
        "  LEFT JOIN atlas_driver_offer_bpp.person p ON p.id = srfd.driver_id "
        f" WHERE sr.transaction_id = '{transaction_id}'")
    return [tuple(r.split("|")) for r in rows]


def stored_shortlist(transaction_id):
    rows = sql("SELECT coalesce(chosen_drivers, '(null)') "
               "  FROM atlas_driver_offer_bpp.search_request "
               f" WHERE transaction_id = '{transaction_id}'")
    return rows[0] if rows else "(no search_request row)"


def run(token, shortlist, label):
    print(f"\n=== {label}")
    sid, est = search(token)
    if not est:
        print(f"   no {VARIANT} estimate — cannot test")
        return None
    body = {"autoAssignEnabled": False}
    if shortlist:
        body["chosenDrivers"] = ",".join(shortlist)
    code, _ = call("POST", f"{RIDER}/v2/estimate/{est['id']}/select2", body, token=token)
    print(f"   select2 -> HTTP {code}   sent chosenDrivers={body.get('chosenDrivers', '(none)')}")
    time.sleep(8)
    print(f"   stored on search_request.chosen_drivers: {stored_shortlist(sid)}")
    rows = asked(sid)
    print(f"   drivers actually asked: {len(rows)}")
    for d, name in rows:
        print(f"      {name or '(no name)':<12} {d}")
    return rows


def main():
    token = sign_in()
    print(f"signed in as {NUM}")

    code, body = call("GET", f"{SHIM}/fleet/nearby?lat={LAT}&lon={LON}&variant={VARIANT}",
                      token=token)
    fleet = body.get("drivers", [])
    print(f"{len(fleet)} {VARIANT} drivers nearby, per the shim")
    if not fleet:
        print("FAILED: no drivers to choose from. Run ./setup.sh drivers first.")
        return 1

    control = run(token, None, "A. no shortlist — everyone eligible should be asked")
    if control is None:
        return 1
    if len(control) < 2:
        print("\n   WARNING: the control asked fewer than 2 drivers, so B proving")
        print("   '1' would prove nothing. Get more drivers online and re-run.")

    pick = fleet[0]
    chosen = run(token, [pick["id"]], f"B. shortlist of one — only {pick['id']} should be asked")
    if chosen is None:
        return 1

    print("\n=== verdict")
    ok = True
    if len(chosen) != 1:
        print(f"   FAIL: asked {len(chosen)} drivers, expected exactly 1")
        ok = False
    elif chosen[0][0] != pick["id"]:
        print(f"   FAIL: asked {chosen[0][0]}, expected {pick['id']}")
        ok = False
    else:
        print(f"   OK — only the chosen driver was asked ({pick.get('name')})")
    if len(control) <= len(chosen):
        print(f"   INCONCLUSIVE: control asked {len(control)}, shortlist asked {len(chosen)}")
        ok = False
    else:
        print(f"   OK — control asked {len(control)}, shortlist asked {len(chosen)}")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
