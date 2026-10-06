#!/usr/bin/env python3
"""Does the dispatch restriction list say what it should, and reach Redis?

Run it ON the VPS:

    python3 probe-restricted-drivers.py

── What this can and cannot prove ──────────────────────────────────────────
It proves the half that lives in our hands: the policy query picks the right
drivers, and the result reaches the exact Redis key the driver binary reads.

It cannot prove the binary honours it — that is the Haskell patch, and until a
build carrying it is deployed the key sits there being read by nobody. Which is
the point of publishing first: the list can be wrong for a week and nothing
happens, so it is worth being sure of before the binary starts obeying it.

── The key is the whole integration ────────────────────────────────────────
`dynamic-offer-driver-app:movin:restricted`. The prefix is Hedis's, not ours.
Get it wrong and nothing fails anywhere: the binary reads a missing key,
restricts nobody, and the feature is silently off for ever. So the name is
asserted here rather than assumed.

Everything it changes is restored.
"""
import json
import subprocess
import sys

KEY = "dynamic-offer-driver-app:movin:restricted"

passed = 0
failed = 0


def check(name, ok, detail=""):
    global passed, failed
    if ok:
        passed += 1
        print(f"  PASS  {name}")
    else:
        failed += 1
        print(f"  FAIL  {name}   {detail}")


def psql(sql):
    r = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres",
         "-d", "atlas_dev", "-tAc", sql],
        capture_output=True, text=True, timeout=60)
    if r.returncode != 0:
        print("psql:", r.stderr.strip(), file=sys.stderr)
    return r.stdout.strip()


def redis_get(key):
    r = subprocess.run(["docker", "exec", "ny-redis", "redis-cli", "GET", key],
                       capture_output=True, text=True, timeout=30)
    return r.stdout.strip()


def published():
    raw = redis_get(KEY)
    try:
        return json.loads(raw) if raw else None
    except ValueError:
        return None


def restart_shim():
    """The shim republishes on startup. Cheaper than waiting out its timer,
    and it also exercises the startup path every run."""
    subprocess.run(["docker", "compose", "restart", "maps-shim"],
                   cwd="/opt/ny/local-stack", capture_output=True, timeout=120)
    subprocess.run(["sleep", "4"], timeout=20)


# The same policy the shim applies, so a disagreement is a real disagreement
# rather than two different questions.
POLICY = """
  WITH period AS (
    SELECT s.driver_id, s.paid_until,
           coalesce((SELECT max(sp.covers_from) FROM movin.subscription_payment sp
                      WHERE sp.driver_id = s.driver_id AND sp.applied_at IS NOT NULL
                        AND sp.covers_until > now()), s.created_at) AS started
      FROM movin.subscription s)
  SELECT p.id FROM atlas_driver_offer_bpp.person p
    LEFT JOIN period pd ON pd.driver_id = p.id
   WHERE p.role = 'DRIVER'
     AND (pd.paid_until IS NULL OR pd.paid_until <= now()
          OR (%d > 0 AND (SELECT count(*) FROM atlas_driver_offer_bpp.ride r
                           WHERE r.driver_id = p.id AND r.status = 'COMPLETED'
                             AND r.created_at >= pd.started) >= %d))
   ORDER BY p.id"""


def policy(cap):
    out = psql(POLICY % (cap, cap))
    return [x for x in out.split("\n") if x]


print("== what is published now")
now = published()
check("the key exists and is a JSON array", isinstance(now, list), f"got {now!r}")
print(f"   {len(now) if isinstance(now, list) else '?'} restricted")

expected = policy(300)
check("it matches the policy query", sorted(now or []) == sorted(expected),
      f"redis {sorted(now or [])[:3]} vs sql {sorted(expected)[:3]}")

# ── a driver whose month has run out ────────────────────────────────────────
driver = psql("SELECT driver_id FROM movin.subscription ORDER BY driver_id LIMIT 1")
if not driver:
    print("\n   no subscription rows — apply driver-subscription.sql first")
    sys.exit(2)
before = psql(f"SELECT paid_until::text FROM movin.subscription WHERE driver_id = '{driver}'")
print(f"\n== lapsing {driver}")
print(f"   (paid_until {before}, restored at the end)")

try:
    psql(f"UPDATE movin.subscription SET paid_until = now() - interval '1 day' "
         f"WHERE driver_id = '{driver}'")
    check("the policy now names him", driver in policy(300))
    restart_shim()
    after = published()
    check("and the published list names him", driver in (after or []),
          f"list is {after}")

    # ── the ride cap, without touching the deployed setting ─────────────────
    #
    # SUBSCRIPTION_RIDE_CAP is 300 and the busiest driver has done 15, so real
    # data will never exercise this arm. Running the same query with a cap of 1
    # proves it works and a cap of 0 proves it can be switched off.
    #
    # **The period is what makes this test non-obvious**, and the first version
    # of it was wrong. Rides are counted from the start of the month he is
    # currently inside — for the free month, that is the day the row was
    # created, which is today. So no existing ride falls within any driver's
    # current period, and a cap of 1 correctly catches nobody. To exercise the
    # arm the period has to be moved back over rides that exist.
    print("\n== the ride cap arm")
    psql(f"UPDATE movin.subscription SET paid_until = '{before}' WHERE driver_id = '{driver}'")

    busy = psql("SELECT r.driver_id FROM atlas_driver_offer_bpp.ride r "
                "JOIN movin.subscription s ON s.driver_id = r.driver_id "
                "WHERE r.status = 'COMPLETED' GROUP BY r.driver_id "
                "ORDER BY count(*) DESC LIMIT 1")
    if not busy:
        print("   no driver has a completed ride — cap arm not exercised")
    else:
        rides = psql(f"SELECT count(*) FROM atlas_driver_offer_bpp.ride "
                     f"WHERE driver_id = '{busy}' AND status = 'COMPLETED'")
        was_created = psql(f"SELECT created_at::text FROM movin.subscription "
                           f"WHERE driver_id = '{busy}'")
        print(f"   {busy} has {rides} completed rides")
        try:
            # Open his period wide enough to contain them.
            psql(f"UPDATE movin.subscription SET created_at = now() - interval '400 days' "
                 f"WHERE driver_id = '{busy}'")
            check("a cap of 1 now catches him", busy in policy(1),
                  f"caught {len(policy(1))} drivers")
            check("a cap of 0 does not", busy not in policy(0))
            check("the real cap of 300 does not either", busy not in policy(300),
                  f"{rides} rides should be far below 300")
        finally:
            psql(f"UPDATE movin.subscription SET created_at = '{was_created}' "
                 f"WHERE driver_id = '{busy}'")

finally:
    psql(f"UPDATE movin.subscription SET paid_until = '{before}' WHERE driver_id = '{driver}'")
    restart_shim()
    back = published()
    print(f"\n   restored; {driver} paid_until {before}")
    print(f"   published list is now {back}")

print(f"\n{passed} passed, {failed} failed")
sys.exit(1 if failed else 0)
