#!/usr/bin/env python3
"""
« Lieux proposés », day 6, point 2: the security review of /places/, LIVE and
SIGNED IN -- the part probe-place-photos.sh could not do (it has no account).

Runs ON the server (ufw limits SSH, and from the laptop the guard rate-limits):

    scp investigations/probe-places-live.py ny:/tmp/
    ssh ny 'python3 /tmp/probe-places-live.py' | tee ~/day6-places-security.log

Every line is `ok` or `BAD`, then the request that gave it, as a curl command
(tokens shown as $TOKEN_A / $TOKEN_B, never printed). The last lines list the
places it made, for the day-6 erase.

── Who it signs in as ─────────────────────────────────────────────────────────
The ride test's two passengers, A = +222 22778899 (Mauritania) and
B = +213 0555000199 (Algeria): test accounts kept until the launch clean-up
(CLAUDE.md, *The test fleet*). Signed in on loopback 8013 with the backend's
fixed code 7891, exactly as ops/checks/probe-two-country-rides.py does: no SMS,
no WhatsApp, and the guard is not involved. Every /places/ call then goes
through the PUBLIC address, as a phone's does.

── What it writes, and only that ──────────────────────────────────────────────
A's proposals, « Test jour 6 sécurité 1..5 » in Nouakchott, pending, and two
small photos on the first. Everything else it sends is refused before anything
is stored, and it checks that (A's « Mes lieux » count before and after).
A then cannot propose for 24 hours: that is the limit being proven.

── Why each answer ────────────────────────────────────────────────────────────
maps-shim/suggested-places.js and tests/suggested-places-routes.test.js, where
every line here is already proven against the real table (sections 1, 2, 5, 6,
9, 9b-9d). This run proves the live server says the same.

    EDGE=https://… python3 probe-places-live.py     another public address
"""
import calendar
import json
import os
import sys
import time
import urllib.error
import urllib.request

EDGE = os.environ.get("EDGE", "https://api.movinapp.net").rstrip("/")
RIDER = "http://localhost:8013"
OTP = "7891"
A_NUM = ("+222", "22778899")
B_NUM = ("+213", "0555000199")

# Nouakchott, Tevragh Zeina, and points 300 m apart northwards: far enough
# that no two proposals are « the same name within 100 m », and inside the
# Mauritanian service area.
NKC = (18.0930, -15.9750)
STEP = 0.0027  # ~300 m of latitude
RABAT = (34.0209, -6.8416)  # inside the box around both countries, in neither
PARIS = (48.8566, 2.3522)   # outside the box itself

# The smallest bytes each check needs: the shim recognises a photo by its
# first bytes (PHOTO_MAGIC), nothing else.
JPEG = bytes([0xFF, 0xD8, 0xFF, 0xE0]) + b"\x00" * 60
BIG = bytes([0xFF, 0xD8, 0xFF, 0xE0]) + b"\x00" * (513 * 1024)

results = []
made = []


def http(method, url, body=None, token=None, raw=None, ctype=None):
    """(status, json-or-text). 0 when no answer came."""
    data = raw if raw is not None else (json.dumps(body).encode() if body is not None else None)
    req = urllib.request.Request(url, data=data, method=method)
    if data is not None:
        req.add_header("Content-Type", ctype or "application/json")
    if token:
        req.add_header("token", token)
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            text, code = r.read().decode(errors="replace"), r.status
    except urllib.error.HTTPError as e:
        text, code = e.read().decode(errors="replace"), e.code
    except Exception as e:  # noqa: BLE001 -- the line says what happened
        return 0, str(e)
    try:
        return code, json.loads(text)
    except Exception:  # noqa: BLE001
        return code, text
    finally:
        time.sleep(0.3)  # well under the edge's 240 a minute


def curl(method, path, tok=None, body=None, raw_note=None):
    parts = [f"curl -s -X {method}"]
    if tok:
        parts.append(f"-H \"token: ${tok}\"")
    if body is not None:
        parts.append(f"-H 'Content-Type: application/json' -d '{json.dumps(body, ensure_ascii=False)}'")
    if raw_note:
        parts.append(raw_note)
    parts.append(f"{EDGE}{path}")
    return " ".join(parts)


def check(label, ok, got, cmd):
    results.append(ok)
    print(f"{'ok ' if ok else 'BAD'}  {label}")
    print(f"       got: {str(got)[:160]}")
    print(f"       $ {cmd}")


def err(j):
    return j.get("error") if isinstance(j, dict) else None


def sign_in(cc, number):
    code, a = http("POST", f"{RIDER}/v2/auth",
                   {"mobileCountryCode": cc, "mobileNumber": number, "merchantId": "YATRI"})
    if not isinstance(a, dict) or "authId" not in a:
        sys.exit(f"sign-in {cc} {number}: {code} {str(a)[:200]}")
    code, v = http("POST", f"{RIDER}/v2/auth/{a['authId']}/verify",
                   {"otp": OTP, "deviceToken": "places-security-probe"})
    tok = v.get("token") if isinstance(v, dict) else None
    if not tok:
        sys.exit(f"verify {cc} {number}: {code} {str(v)[:200]}")
    return tok


def mine(tok):
    code, j = http("GET", f"{EDGE}/places/mine", token=tok)
    if code != 200 or not isinstance(j, dict):
        sys.exit(f"/places/mine: {code} {str(j)[:200]}")
    return j.get("places", [])


def place(name, typ, lat, lon):
    return {"name": name, "type": typ, "lat": lat, "lon": lon}


def main():
    print(f"== /places/ on {EDGE}, {time.strftime('%Y-%m-%d %H:%M:%S %Z')}")

    print("\n== 1. no token, and a forged one: 401 everywhere, before anything is read")
    for method, path, body, raw in [
        ("GET", "/places/mine", None, None),
        ("POST", "/places/suggest", place("Test jour 6 sécurité 0", "pharmacy", *NKC), None),
        ("GET", f"/places/nearby?type=pharmacy&lat={NKC[0]}&lon={NKC[1]}", None, None),
        ("PUT", "/places/photos/u1/1", None, JPEG),
        ("DELETE", "/places/photos/u1/1", None, None),
    ]:
        code, j = http(method, EDGE + path, body=body, raw=raw, ctype="image/jpeg" if raw else None)
        check(f"{method} {path.split('?')[0]}, no token: 401", code == 401, f"{code} {j}",
              curl(method, path, body=body, raw_note="--data-binary @small.jpg" if raw else None))
    code, j = http("GET", EDGE + "/places/mine", token="not-a-real-token")
    check("GET /places/mine, a forged token: 401", code == 401, f"{code} {j}",
          curl("GET", "/places/mine") + " -H 'token: not-a-real-token'")

    print("\n== signing in A (+222 22778899) and B (+213 0555000199) on loopback, code 7891")
    a = sign_in(*A_NUM)
    b = sign_in(*B_NUM)
    print("   both signed in")
    a_before = len(mine(a))
    b_before_ids = {p["placeId"] for p in mine(b)}
    print(f"   A has {a_before} proposal(s) already; B has {len(b_before_ids)}")

    print("\n== 2. what a proposal must be: refused, nothing stored")
    for label, body, want_code, want_err in [
        ("a name of 1 character", place("x", "pharmacy", *NKC), 400, "bad_name"),
        ("a name of 81 characters", place("T" * 81, "pharmacy", *NKC), 400, "bad_name"),
        ("an unknown type", place("Test jour 6 sécurité 0", "spaceship", *NKC), 400, "bad_type"),
        ("a pin outside the box (Paris)", place("Test jour 6 sécurité 0", "pharmacy", *PARIS), 400, "bad_position"),
        ("a pin in neither country (Rabat)", place("Test jour 6 sécurité 0", "pharmacy", *RABAT), 422, "outside_service_area"),
    ]:
        code, j = http("POST", EDGE + "/places/suggest", body=body, token=a)
        check(f"{label}: {want_code} {want_err}", code == want_code and err(j) == want_err,
              f"{code} {j}", curl("POST", "/places/suggest", "TOKEN_A", body))
    path = f"/places/nearby?type=other&lat={NKC[0]}&lon={NKC[1]}"
    code, j = http("GET", EDGE + path, token=a)
    check("« other » around me: 400 bad_type", code == 400 and err(j) == "bad_type", f"{code} {j}",
          curl("GET", path, "TOKEN_A"))
    n = len(mine(a))
    check("and none of those stored anything (A's « Mes lieux » unchanged)", n == a_before,
          f"{a_before} -> {n}", curl("GET", "/places/mine", "TOKEN_A"))

    print("\n== 3. mine is mine: A proposes, B cannot see it or touch its photos")
    body = place("Test jour 6 sécurité 1", "pharmacy", *NKC)
    code, j = http("POST", EDGE + "/places/suggest", body=body, token=a)
    pid = j.get("placeId") if isinstance(j, dict) else None
    check("A proposes « Test jour 6 sécurité 1 »: 201, pending", code == 201 and bool(pid),
          f"{code} {j}", curl("POST", "/places/suggest", "TOKEN_A", body))
    if not pid:
        return finish()
    made.append(pid)
    check("it is in A's « Mes lieux »", any(p["placeId"] == pid for p in mine(a)), pid,
          curl("GET", "/places/mine", "TOKEN_A"))
    check("it is NOT in B's", all(p["placeId"] != pid for p in mine(b)), pid,
          curl("GET", "/places/mine", "TOKEN_B"))
    near = f"/places/nearby?type=pharmacy&lat={NKC[0]}&lon={NKC[1]}"
    code, j = http("GET", EDGE + near, token=b)
    listed = [p.get("placeId") for p in j.get("results", [])] if isinstance(j, dict) else []
    check("B's « Pharmacie » around it does not list it while it waits", code == 200 and pid not in listed,
          f"{code}, {len(listed)} place(s)", curl("GET", near, "TOKEN_B"))
    p1 = f"/places/photos/{pid}/1"
    code, j = http("PUT", EDGE + p1, raw=JPEG, token=a, ctype="image/jpeg")
    check("A adds photo 1: 200", code == 200, f"{code} {j}",
          curl("PUT", p1, "TOKEN_A", raw_note="--data-binary @small.jpg"))
    code, j = http("PUT", EDGE + f"/places/photos/{pid}/2", raw=JPEG, token=b, ctype="image/jpeg")
    check("B adds a photo to A's place: 404 no_such_place (as if it did not exist)",
          code == 404 and err(j) == "no_such_place", f"{code} {j}",
          curl("PUT", f"/places/photos/{pid}/2", "TOKEN_B", raw_note="--data-binary @small.jpg"))
    code, j = http("DELETE", EDGE + p1, token=b)
    check("B deletes A's photo 1: 404 no_such_place", code == 404 and err(j) == "no_such_place",
          f"{code} {j}", curl("DELETE", p1, "TOKEN_B"))
    ph = next((p.get("photos") for p in mine(a) if p["placeId"] == pid), None)
    check("A's photo 1 is still there", ph == [1], ph, curl("GET", "/places/mine", "TOKEN_A"))

    print("\n== 4. the photos: two at most, 512 KB at most")
    code, j = http("PUT", EDGE + f"/places/photos/{pid}/2", raw=JPEG, token=a, ctype="image/jpeg")
    check("A adds photo 2: 200", code == 200, f"{code} {j}",
          curl("PUT", f"/places/photos/{pid}/2", "TOKEN_A", raw_note="--data-binary @small.jpg"))
    code, j = http("PUT", EDGE + f"/places/photos/{pid}/3", raw=JPEG, token=a, ctype="image/jpeg")
    check("a photo 3: 404, refused before anything is read", code == 404, f"{code} {j}",
          curl("PUT", f"/places/photos/{pid}/3", "TOKEN_A", raw_note="--data-binary @small.jpg"))
    code, j = http("PUT", EDGE + p1, raw=BIG, token=a, ctype="image/jpeg")
    check("a photo over 512 KB: 413", code == 413, f"{code} {str(j)[:80]}",
          curl("PUT", p1, "TOKEN_A", raw_note="--data-binary @513k.jpg"))
    ph = next((p.get("photos") for p in mine(a) if p["placeId"] == pid), None)
    check("A's place still has exactly photos 1 and 2", ph == [1, 2], ph,
          curl("GET", "/places/mine", "TOKEN_A"))

    print("\n== 5. five a day: the sixth in 24 hours is refused")
    day = time.time() - 24 * 3600
    recent = sum(1 for p in mine(a)
                 if calendar.timegm(time.strptime(p["createdAt"][:19], "%Y-%m-%dT%H:%M:%S")) > day)
    print(f"   A has {recent} proposal(s) in the last 24 hours, this run's included")
    codes = []
    for i in range(2, 2 + max(0, 5 - recent) + 1):
        body = place(f"Test jour 6 sécurité {i}", "pharmacy", NKC[0] + STEP * (i - 1), NKC[1])
        code, j = http("POST", EDGE + "/places/suggest", body=body, token=a)
        codes.append(code)
        if code == 201 and isinstance(j, dict) and j.get("placeId"):
            made.append(j["placeId"])
        last, last_body = (code, j), body
    want = [201] * max(0, 5 - recent) + [429]
    check(f"proposals up to five pass, the next is refused: {want}",
          codes == want and err(last[1]) == "daily_limit", f"{codes}, last: {last[1]}",
          curl("POST", "/places/suggest", "TOKEN_A", last_body))
    finish()


def finish():
    bad = results.count(False)
    print(f"\n== {len(results) - bad} ok, {bad} BAD")
    print(f"== places this run made (pending, A's): {','.join(made) or 'none'}")
    sys.exit(1 if bad else 0)


if __name__ == "__main__":
    main()
