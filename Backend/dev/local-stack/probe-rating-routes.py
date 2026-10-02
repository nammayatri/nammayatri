#!/usr/bin/env python3
"""Do the two rating routes answer, through the public edge?

Run this ON the VPS. A probe fired from the laptop meets the rate-limit guard,
and a lone timeout from there is not a result -- it has produced three false
verdicts on this project already.

Both routes are designed never to 404 and never to throw: an unrated person is
the normal state, so `{"rating": null, "total": 0}` is a PASS, not a miss. What
is being proven here is that nginx routes it, the shim answers it, and the
Postgres join runs -- not that anybody has been rated yet.
"""
import json
import subprocess
import sys
import urllib.error
import urllib.request

BASE = "https://api.169-58-139-65.sslip.io"


def get(path):
    q = urllib.request.Request(BASE + path, method="GET")
    try:
        with urllib.request.urlopen(q, timeout=20) as r:
            return r.read().decode(), r.status
    except urllib.error.HTTPError as e:
        return e.read().decode(), e.code
    except Exception as e:
        return str(e), 0


def psql(sql):
    r = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres", "-d", "atlas_dev", "-tAc", sql],
        capture_output=True, text=True, timeout=60)
    return r.stdout.strip()


ok = True

# A rider who exists. Any number does -- an unknown one must answer 200 too.
for number in ("0555000199", "0000000000"):
    raw, code = get(f"/rating/phone/{number}")
    good = code == 200 and "rating" in raw
    ok &= good
    print(f"  /rating/phone/{number:<12} -> {code}  {raw[:70]}  {'ok' if good else 'FAIL'}")

# ── A driver who has actually been rated, and that is the whole point ────────
# Both routes answer {"rating": null, "total": 0} for somebody unrated AND on
# any internal failure -- deliberately, so a settings screen never shows an
# error over a star. Which means a probe that only ever sees nulls proves
# nothing: a query that had silently stopped working would look identical.
#
# That is not hypothetical here. The passenger avatar key was compared on the
# wrong number format for days; every upload succeeded, every lookup found
# nothing, and nothing anywhere failed. So this asks about somebody the
# database says has a rating, and fails if the route disagrees.
rated = psql("SELECT id FROM atlas_driver_offer_bpp.person "
             "WHERE role = 'DRIVER' AND rating IS NOT NULL "
             "ORDER BY rating DESC LIMIT 1;")

if not rated:
    print("\n  no driver carries a rating -- cannot prove the join. Rate one first.")
    ok = False
else:
    truth = psql("SELECT p.rating::text || '|' || count(r.id) "
                 "FROM atlas_driver_offer_bpp.person p "
                 "LEFT JOIN atlas_driver_offer_bpp.rating r ON r.driver_id = p.id "
                 f"WHERE p.id = '{rated}' GROUP BY p.rating;")
    want_rating, want_total = truth.split("|")
    raw, code = get(f"/rating/driver/{rated}")
    body = json.loads(raw) if code == 200 else {}
    agrees = (
        code == 200
        and body.get("rating") is not None
        and abs(float(body["rating"]) - float(want_rating)) < 0.01
        and int(body.get("total", -1)) == int(want_total)
    )
    ok &= agrees
    print(f"\n  a driver who HAS a rating:")
    print(f"    database -> {want_rating} over {want_total} ratings")
    print(f"    route    -> {code}  {raw[:70]}  {'ok' if agrees else 'FAIL'}")

# ── And the passenger half, which is the one the client asked for ───────────
# The route is keyed by her phone number, so this walks the join backwards: find
# a rider_details row that carries a rating, follow its phone-number hash to the
# rider-side person who shares it, and ask the route about that number.
#
# Backwards on purpose. Walking it forwards would use the same query the route
# uses, and would pass even if the two schemas had stopped agreeing on the hash
# -- which is the failure that would take every passenger rating with it.
her = psql(
    "SELECT p.unencrypted_mobile_number || '|' || rd.rating::text || '|' || rd.total_ratings "
    "FROM atlas_driver_offer_bpp.rider_details rd "
    "JOIN atlas_app.person p ON p.mobile_number_hash = rd.mobile_number_hash "
    "WHERE rd.rating IS NOT NULL "
    "ORDER BY rd.total_ratings DESC LIMIT 1;")

if not her:
    print("\n  no passenger carries a rating that maps to a rider row -- "
          "the phone route cannot be proved yet")
    ok = False
else:
    number, want_rating, want_total = her.split("|")
    raw, code = get(f"/rating/phone/{number}")
    body = json.loads(raw) if code == 200 else {}
    agrees = (
        code == 200
        and body.get("rating") is not None
        and abs(float(body["rating"]) - float(want_rating)) < 0.01
        and int(body.get("total", -1)) == int(want_total)
    )
    ok &= agrees
    print(f"\n  a passenger who HAS a rating:")
    print(f"    database -> {want_rating} over {want_total} ratings, number {number}")
    print(f"    route    -> {code}  {raw[:70]}  {'ok' if agrees else 'FAIL'}")

    # The app holds a bare NSN and the database the trunk zero. The route
    # compares the last nine digits for exactly this reason, and that fix was
    # made only after every passenger avatar had silently gone missing.
    nsn = number.lstrip("0")[-9:]
    raw2, code2 = get(f"/rating/phone/{nsn}")
    same = raw2 == raw
    ok &= same
    print(f"    same number as the app holds it ({nsn}) -> {code2}  {raw2[:40]}  "
          f"{'ok' if same else 'FAIL — the trunk-zero bug is back'}")

print("\nBOTH ROUTES ANSWER, AND THE JOIN IS REAL" if ok else "\nSOMETHING IS WRONG")
sys.exit(0 if ok else 1)
