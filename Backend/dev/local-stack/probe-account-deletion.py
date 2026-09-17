#!/usr/bin/env python3
"""Drive the whole deletion-request cycle with a real token.

Refusing an anonymous caller proves the door is shut; it proves nothing about
the room. This walks the states the app actually renders — none, pending,
already-requested, withdrawn — and checks the row afterwards, because the
withdrawal is a status change and not a delete: the office should be able to
see that somebody asked and changed their mind.

RUNS ON THE VPS. The rider backend is loopback 8013 and the shim is 8030;
neither is reachable from outside.

One sign-in only. The backend has its own auth rate limit — HITS_LIMIT_EXCEED,
"try again in 600 sec" — which fires on the third attempt and is not nginx.
"""
import json
import subprocess
import sys
import urllib.error
import urllib.request

RIDER = "http://localhost:8013"
SHIM = "http://localhost:8030"
PATH = "/account/deletion-request"
NUM, MERCHANT, OTP = "0555000199", "YATRI", "7891"

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


print("=" * 72)
print("ACCOUNT DELETION — the whole cycle, with a real token")
print("=" * 72)

print("\n--- signing a rider in")
code, raw, a = call(f"{RIDER}/v2/auth", "POST",
                    {"mobileNumber": NUM, "mobileCountryCode": "+213", "merchantId": MERCHANT})
if not a or "authId" not in a:
    sys.exit(f"auth failed {code}: {raw[:200]}")
code, raw, v = call(f"{RIDER}/v2/auth/{a['authId']}/verify", "POST",
                    {"otp": OTP, "deviceToken": "deletion-probe"})
TOKEN = (v or {}).get("token")
if not TOKEN:
    sys.exit(f"verify failed {code}: {raw[:200]}")
print("  ok")

# Start from a known place. A leftover request from an earlier run would make
# every assertion below read backwards, and "the test was dirty" is the most
# expensive kind of false failure in this repository.
person = pg(f"SELECT id FROM atlas_app.person WHERE unencrypted_mobile_number = '{NUM}' LIMIT 1")
pg(f"DELETE FROM movin.deletion_request WHERE person_id = '{person}'")
print(f"  subject {person}, any earlier requests cleared")

print("\n--- 1. before asking")
code, raw, s = call(f"{SHIM}{PATH}", token=TOKEN)
check(code == 200, "status answers 200", f"got {code}")
check((s or {}).get("state") == "none", "state is `none`", raw[:120])

print("\n--- 2. asking")
code, raw, r = call(f"{SHIM}{PATH}", "POST", {"reason": "Je change de numéro"}, token=TOKEN)
check(code == 200 and (r or {}).get("ok") is True, "request accepted", f"{code} {raw[:120]}")
check(bool((r or {}).get("requestedAt")) and bool((r or {}).get("deleteBy")),
      "both dates come back", raw[:120])

print("\n--- 3. the app reopens the screen")
code, raw, s = call(f"{SHIM}{PATH}", token=TOKEN)
check((s or {}).get("state") == "pending", "state is `pending`", raw[:120])
check((s or {}).get("deleteBy") == (r or {}).get("deleteBy"),
      "the date does not move between reads", raw[:120])

print("\n--- 4. asking twice (a slow connection, two taps)")
code, raw, again = call(f"{SHIM}{PATH}", "POST", {}, token=TOKEN)
check(code == 409, "second request is refused with 409", f"got {code}")
check((again or {}).get("blocker") == "already_requested", "and names the reason", raw[:120])
rows = pg(f"SELECT count(*) FROM movin.deletion_request WHERE person_id = '{person}'")
check(rows == "1", "still exactly one row", f"got {rows}")

print("\n--- 5. what the office sees")
queued = pg(f"SELECT side || ' | ' || coalesce(phone,'no phone') || ' | ' || coalesce(reason,'-') "
            f"FROM movin.deletion_queue WHERE person_id = '{person}'")
check(bool(queued), "the request is in `deletion_queue`", "empty")
print(f"      {queued}")
check("Je change de numéro" in queued, "the reason survived unicode intact", queued)

print("\n--- 6. changing her mind")
code, raw, w = call(f"{SHIM}{PATH}", "DELETE", token=TOKEN)
check(code == 200 and (w or {}).get("ok") is True, "withdrawal accepted", f"{code} {raw[:120]}")
code, raw, s = call(f"{SHIM}{PATH}", token=TOKEN)
check((s or {}).get("state") == "none", "state is `none` again", raw[:120])

status = pg(f"SELECT status FROM movin.deletion_request WHERE person_id = '{person}'")
check(status == "withdrawn", "the row is kept, marked `withdrawn`", f"got {status!r}")
check(pg(f"SELECT count(*) FROM movin.deletion_queue WHERE person_id = '{person}'") == "0",
      "and it has left the queue", "still queued")

print("\n--- 7. and she can ask again afterwards")
code, raw, r2 = call(f"{SHIM}{PATH}", "POST", {}, token=TOKEN)
check(code == 200 and (r2 or {}).get("ok") is True, "a fresh request is accepted", f"{code} {raw[:120]}")
check(pg(f"SELECT count(*) FROM movin.deletion_request WHERE person_id = '{person}'") == "2",
      "two rows now: the history is kept", "")

# Leave nothing behind. This is a live pilot, not a fixture.
pg(f"DELETE FROM movin.deletion_request WHERE person_id = '{person}'")
print("\n  probe rows removed")

print("\n" + "=" * 72)
print(f"{PASS} passed, {FAIL} failed")
sys.exit(1 if FAIL else 0)
