#!/usr/bin/env python3
"""Play a driver, so the passenger app can be built and demonstrated on one phone.

    ./simulate-driver.py seed       # one Algerian driver per sellable variant
    ./simulate-driver.py status     # who exists, who is online, how fresh
    ./simulate-driver.py once       # take the next request, drive it, finish
    ./simulate-driver.py daemon     # all drivers online, keep accepting
    ./simulate-driver.py finish     # close out any ride left hanging

Runs ON THE SERVER. The driver API is loopback-only (`/ui/` is not published,
and publishing it would let anyone become a driver, because driver auth
self-creates accounts on a fixed OTP). So this is `ssh ny`, not a laptop script.

── Why this exists ─────────────────────────────────────────────────────────
Screens 10-13 cannot be built or shown without a driver on the other side, and
there is no driver app. This drives the real endpoints against the real backend,
so what the passenger app sees is what it will see in production.

── What it is honest about ─────────────────────────────────────────────────
It reads the ride OTP out of Postgres. A real driver is told the code by the
passenger; `/ui/driver/ride/list` deliberately does not carry it. That one
shortcut is the whole difference between this and a real driver, and it is
worth keeping visible rather than hiding behind an API that looks complete.

── Traps this is written around, each already paid for ─────────────────────
  * Dispatch matches on VEHICLE VARIANT. A rider picking Economy reaches only
    HATCHBACK drivers. Before `seed`, only SEDAN existed, so two of the three
    rows in the app spun for 300s and returned nothing, with no error on either
    side. That is why `seed` exists and why `once` warns about coverage.
  * A position whose `ts` is not newer than the stored one is DROPPED, and the
    server still answers 200. Timestamps here are forced strictly increasing.
  * driver_location carries lat, lon AND a PostGIS `point`; the pool tests
    `point`. Never write position with SQL -- POST it, which sets all three.
  * A driver whose position goes stale is invisible to the pool. Idle drivers
    here keep a heartbeat going, so this replaces drivers-keepalive.sh for the
    drivers it owns.
  * `offeredFare` is the EXTRA fee on top of baseFare, not the total. Sending
    the total gives EXTRA_FEE_NOT_ALLOWED.
  * A ride left unfinished LOCKS THAT RIDER OUT. Any later confirm answers
    `E400 INVALID_REQUEST: ACTIVE_BOOKING_PRESENT`, so one abandoned test ride
    ends every future booking for that account. `finish` exists for this, and
    it is worth running after any session that was interrupted.
  * Signing in as a driver is RATE-LIMITED, and this daemon holds a session per
    driver. A probe that logs in as one of the six numbers below revokes that
    session, the daemon signs back in, and the pair can trip
    `HITS_LIMIT_EXCEED` -- ten minutes with no fleet. **Probes must use a
    driver this script does not own.** `login` now waits out the limit instead
    of exiting, because exiting made it worse: systemd restarted into another
    attempt against a limit that counts attempts.
"""
import argparse
import json
import re
import signal
import subprocess
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime, timedelta, timezone

DRIVER_API = "http://localhost:8017"
OSRM = "http://localhost:5000"
OTP = "7891"
CC = "+222"

# How far back to look for a ride this driver should be driving. Longer than
# any rider takes to choose an offer; shorter than an abandoned ride from a
# previous session.
RECENT_RIDE_S = 1800

# The three rows the passenger app sells, and the drivers who play them.
# AUTO_RICKSHAW is excluded on purpose: it is two thirds of the upstream seed
# fleet and there are no auto-rickshaws in Algeria, so the app hides it.
#
# TWO PER ROW, not one. With a single driver per variant, that whole category
# went dead the moment he picked up a ride -- a rider choosing Economy got the
# full 300-second wait and no error. It also meant screen 10 could only ever
# show a list of one offer, so the screen the client actually chose had never
# been exercised as designed.
#
# 0551234567 keeps his SEDAN on purpose: he is the driver every earlier probe
# was proven against, and setup.sh's smoke test recreates him on login.
#
# The names matter more than they look. The backend calls a driver it created
# from a phone number "Chauffeur", and three identical Chauffeurs on a demo
# reads as a test rig rather than a service.
#
# Mauritanian since 2026-09-03. These are the rows seed-mauritanian-fleet.sh
# creates and enrols; this file only signs in as them and answers requests.
#
# The plates are the real Mauritanian format: four digits, two letters, then
# the wilaya, and 00 is Nouakchott. **There is no year in a Mauritanian plate**
# -- the Algerian one carried it in the middle group and the app decoded it,
# which is why the passenger no longer sees a car's year.
FLEET = [
    ("22100001", "SEDAN",     "Mohamed",    "Toyota",  "Corolla",      "Blanc", "2145 AC 00"),
    ("22100003", "HATCHBACK", "Cheikh",     "Renault", "Clio",         "Bleu",  "4027 CD 00"),
    ("22100005", "SUV",       "Moustapha",  "Toyota",  "Land Cruiser", "Blanc", "6248 EL 00"),
    ("22100004", "HATCHBACK", "Brahim",     "Dacia",   "Sandero",      "Rouge", "5391 DK 00"),
    ("22100002", "SEDAN",     "Ahmed",      "Hyundai", "Accent",       "Gris",  "3182 BM 00"),
    ("22100006", "SUV",       "Abdallahi",  "Nissan",  "Patrol",       "Noir",  "7135 FM 00"),
]

# Where idle drivers wait: scattered around central Nouakchott, each several
# hundred metres from the usual pickup rather than on top of it. Parked exactly
# on the pickup, the approach leg is zero points and screen 11 has nothing to
# show -- the driver is simply already there.
#
# Parked in Algiers, as these were until 2026-09-03, they are outside the
# geofence and invisible -- the same silence in a different place.
BASE = [
    (18.0902, -15.9615), (18.0812, -15.9548), (18.0871, -15.9663),
    (18.0838, -15.9502), (18.0925, -15.9571), (18.0790, -15.9628),
]

_last_ts = {}


# ──────────────────────────────────────────────────────────────── plumbing
def pg(sql, db="atlas_dev"):
    """psql in the container. No `docker exec -i`: the -i steals our stdin when
    this script is piped in over ssh, and the rest of the file vanishes."""
    o = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres", "-d", db,
         "-At", "-c", sql],
        capture_output=True, text=True, timeout=30)
    if o.returncode != 0:
        raise RuntimeError(f"psql failed: {o.stderr.strip()}")
    return o.stdout.strip()


def pg_soft(sql, why=""):
    """For statements whose constraints this script does not want to assume --
    an ON CONFLICT target that may not be a real unique index, a table whose
    NOT NULL columns differ across schema versions. Reports, does not abort."""
    try:
        return pg(sql)
    except RuntimeError as e:
        say(f"skipped ({why}): {str(e).splitlines()[0][:140]}", 2)
        return None


def call(method, url, body=None, token=None):
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
    except Exception as e:
        return None, 0, str(e)
    try:
        return json.loads(raw), code, raw
    except ValueError:
        return None, code, raw


def say(msg, indent=0):
    print(f"{datetime.now():%H:%M:%S} {'  ' * indent}{msg}", flush=True)


def die(msg):
    print(f"\033[1;31mBAD \033[0m{msg}", file=sys.stderr)
    sys.exit(1)


# ──────────────────────────────────────────────────────────────── the fleet
def merchant_uuid():
    """Driver auth wants the merchant UUID. The rider side wants the short id,
    and getting them the wrong way round returns a bare 'Not found'."""
    m = pg("SELECT id FROM atlas_driver_offer_bpp.merchant "
           "WHERE short_id='NAMMA_YATRI_PARTNER';")
    if not m:
        die("merchant NAMMA_YATRI_PARTNER missing -- driver seed never migrated")
    return m


def login(number, patience=900):
    """Sign a driver in, waiting out the auth rate limit rather than dying on it.

    ── Why this cannot simply die ──────────────────────────────────────────
    The driver API rate-limits sign-ins. Exceed it and `/ui/auth` answers

        HITS_LIMIT_EXCEED -- "Hits limit reached. Try again in 600 sec."

    This used to `die()`, which exits 1, which systemd answers by restarting
    ten seconds later -- into another sign-in attempt, against a limit that is
    counting attempts. **The retry was feeding the thing it was recovering
    from.** Measured 2026-08-18: fifteen crash-restarts in four minutes, and
    because the daemon's `finally` puts drivers offline on the way out, the
    fleet was offline for all of it. A rider watched "3 chauffeurs près de
    vous" and got no offer at all.

    What tripped it was a probe signing in as 0551234567 -- a FLEET driver.
    Each probe login revoked the daemon's session, the daemon signed back in,
    and the two of them raced into the limit. Probes should use a driver the
    daemon does not own; this backoff is the belt to that braces.

    So: wait, out loud, and come back. A daemon that pauses for ten minutes is
    fixed by itself; a daemon that exits is fixed by a person.
    """
    mid = merchant_uuid()
    waited = 0
    while True:
        a, code, raw = call("POST", f"{DRIVER_API}/ui/auth", {
            "mobileNumber": number, "mobileCountryCode": CC, "merchantId": mid})
        if code == 200 and a and "authId" in a:
            break
        if "HITS_LIMIT_EXCEED" in (raw or "") and waited < patience:
            # The server states its own cooldown; trust it rather than guess,
            # and add a little so the retry is not on the exact boundary.
            hold = 60
            found = re.search(r"(\d+)\s*sec", raw or "")
            if found:
                hold = min(int(found.group(1)) + 5, patience - waited)
            say(f"{number}: rate-limited, waiting {hold}s rather than exiting")
            time.sleep(hold)
            waited += hold
            continue
        die(f"auth failed for {number}: {raw[:200]}")

    v, code, raw = call("POST", f"{DRIVER_API}/ui/auth/{a['authId']}/verify",
                        {"otp": OTP, "deviceToken": f"sim-{number}"})
    if code != 200 or not v or "token" not in v:
        die(f"verify failed for {number}: {raw[:200]}")
    return v["token"], v.get("person", {}).get("id") or v.get("id")


def driver_id(number):
    return pg(f"""SELECT id FROM atlas_driver_offer_bpp.person
                   WHERE unencrypted_mobile_number='{number}'
                     AND mobile_country_code='{CC}' AND role='DRIVER';""")


def current_position(number):
    """Where the server believes this driver is, right now.

    Read back rather than remembered: a ride can be picked up on a later pass,
    minutes after the offer was made, by which point any position this script
    was holding is stale.
    """
    did = driver_id(number)
    if not did:
        return None
    row = pg(f"""SELECT lat || ',' || lon FROM atlas_driver_offer_bpp.driver_location
                  WHERE driver_id='{did}';""")
    if not row or "," not in row:
        return None
    lat, lon = row.split(",")
    try:
        return (float(lat), float(lon))
    except ValueError:
        return None


def seed():
    """Make every row the app sells actually servable.

    Creating the driver goes through the API on purpose -- POST /ui/auth calls
    createDriverWithDetails, so registration and login are exercised rather than
    faked. Only the vehicle has to be written directly: there is no driver-side
    endpoint to register one on this binary.
    """
    say("seeding the Algerian test fleet")
    mid = merchant_uuid()
    for i, (num, variant, name, make, model, colour, plate) in enumerate(FLEET):
        did = driver_id(num)
        if not did:
            say(f"{num}: creating (unknown number self-registers a driver)", 1)
            login(num)
            did = driver_id(num)
            if not did:
                die(f"{num}: auth succeeded but no person row appeared")
        else:
            say(f"{num}: exists ({did})", 1)

        # enabled+verified or the pool skips him; blocked is the obvious trap.
        pg(f"""UPDATE atlas_driver_offer_bpp.driver_information
                  SET enabled=true, verified=true, blocked=false
                WHERE driver_id='{did}';""")
        pg_soft(f"""INSERT INTO atlas_driver_offer_bpp.driver_stats (driver_id)
                    VALUES ('{did}') ON CONFLICT (driver_id) DO NOTHING;""",
                "driver_stats")

        # vehicle_class '3WT' is copied from the row that is known to dispatch
        # correctly. It reads wrong for a sedan and is an upstream artifact;
        # dispatch matches on `variant`, so it is left alone deliberately.
        if pg(f"""SELECT count(*) FROM atlas_driver_offer_bpp.vehicle
                   WHERE driver_id='{did}';""") == "0":
            pg(f"""INSERT INTO atlas_driver_offer_bpp.vehicle
                     (driver_id, capacity, make, model, variant, color,
                      registration_no, merchant_id, vehicle_class,
                      created_at, updated_at)
                   VALUES ('{did}', 4, '{make}', '{model}', '{variant}',
                           '{colour}', '{plate}', '{mid}', '3WT', now(), now());""")
        else:
            pg(f"""UPDATE atlas_driver_offer_bpp.vehicle
                      SET variant='{variant}', make='{make}', model='{model}',
                          color='{colour}', registration_no='{plate}',
                          updated_at=now()
                    WHERE driver_id='{did}';""")
        # Named, because "Chauffeur" three times over reads as a test rig.
        pg(f"""UPDATE atlas_driver_offer_bpp.person SET first_name='{name}'
                WHERE id='{did}';""")

        # Position via the API, never SQL: driver_location also carries a
        # PostGIS `point` and the pool tests THAT, not lat/lon.
        tok, _ = login(num)
        post_position(tok, BASE[i % len(BASE)])
        say(f"{num}: {name} — {variant} {make} {model} {colour} [{plate}]", 2)

    say("fleet ready")
    status()


def status():
    print()
    print(pg("""SELECT rpad(coalesce(p.unencrypted_mobile_number,'(none)'),12)
                    || rpad(coalesce(v.variant,'NO VEHICLE'),15)
                    || rpad(CASE WHEN di.active THEN 'online' ELSE 'offline' END, 9)
                    || rpad(CASE WHEN di.on_ride THEN 'ON RIDE' ELSE '-' END, 9)
                    || 'pos ' || coalesce(round(extract(epoch from
                         (now()-dl.coordinates_calculated_at)))::text,'never') || 's old'
                  FROM atlas_driver_offer_bpp.person p
                  JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id=p.id
                  LEFT JOIN atlas_driver_offer_bpp.vehicle v ON v.driver_id=p.id
                  LEFT JOIN atlas_driver_offer_bpp.driver_location dl ON dl.driver_id=p.id
                 WHERE p.mobile_country_code='+222' AND p.role='DRIVER'
                 ORDER BY v.variant;"""))
    missing = [v for _, v, *_ in FLEET if not pg(
        f"""SELECT 1 FROM atlas_driver_offer_bpp.vehicle v
             JOIN atlas_driver_offer_bpp.person p ON p.id=v.driver_id
            WHERE v.variant='{v}' AND p.mobile_country_code='+222' LIMIT 1;""")]
    if missing:
        print(f"\n  \033[1;31mno driver for: {', '.join(missing)}\033[0m"
              f"  -- those rows in the app will wait 300s and return nothing")
        print("  run:  ./simulate-driver.py seed")
    print()


# ────────────────────────────────────────────────────────────────── driving
def post_position(token, pt):
    """A point whose ts is not newer than the stored one is dropped while the
    server still answers 200. So timestamps are forced strictly increasing."""
    now = datetime.now(timezone.utc)
    prev = _last_ts.get(token)
    if prev is not None and now <= prev:
        now = prev + timedelta(milliseconds=500)
    _last_ts[token] = now
    _, code, raw = call("POST", f"{DRIVER_API}/ui/driver/location",
                        [{"pt": {"lat": round(pt[0], 6), "lon": round(pt[1], 6)},
                          "ts": now.isoformat().replace("+00:00", "Z"),
                          "acc": 8.0}], token)
    return code == 200


def route(a, b):
    """OSRM, the same graph the passenger app is routed on."""
    url = (f"{OSRM}/route/v1/driving/{a[1]},{a[0]};{b[1]},{b[0]}"
           "?overview=full&geometries=geojson")
    r, code, raw = call("GET", url)
    if code != 200 or not r or not r.get("routes"):
        say(f"OSRM gave no route ({code}) -- falling back to a straight line", 2)
        return [a, b], 120.0
    rt = r["routes"][0]
    pts = [(c[1], c[0]) for c in rt["geometry"]["coordinates"]]
    return pts, rt["duration"]


def drive(token, frm, to, speed, label):
    """Walk the real route, posting positions as a phone would.

    speed is a multiplier on real time. 0 teleports, which is what CI wants.
    """
    pts, dur = route(frm, to)
    dist = 0.0
    for i in range(1, len(pts)):
        dist += ((pts[i][0] - pts[i - 1][0]) ** 2 +
                 (pts[i][1] - pts[i - 1][1]) ** 2) ** 0.5
    say(f"{label}: {len(pts)} points, {dur/60:.1f} min of real driving", 2)

    if speed <= 0:
        post_position(token, pts[-1])
        say("teleported (speed 0)", 2)
        return

    wall = dur / speed
    tick = 3.0                       # a position every 3s, as a real phone
    steps = max(1, int(wall / tick))
    say(f"driving at {speed}x -> {wall/60:.1f} min, a fix every {tick:.0f}s", 2)
    for s in range(steps + 1):
        idx = min(len(pts) - 1, int(len(pts) * s / max(1, steps)))
        post_position(token, pts[idx])
        if s < steps:
            time.sleep(tick)
    post_position(token, pts[-1])


# ───────────────────────────────────────────────────────────────── the ride
def ride_otp(ride_id):
    """A real driver is told this by the passenger. There is no driver endpoint
    that carries it -- deliberately -- so the simulator reads the table."""
    return pg(f"SELECT otp FROM atlas_driver_offer_bpp.ride WHERE id='{ride_id}';")


class _Expired:
    """This driver's session is gone; whatever was asked, ask again after login.

    ── Why a sentinel and not just None ────────────────────────────────────
    This backend allows ONE SESSION PER USER, drivers included. Anything that
    logs in as a driver -- `finish`, `seed`, a probe, a second copy of this
    script -- silently revokes the token the daemon has been holding, and from
    then on every call it makes returns 401.

    `poll` used to fold that into `return None`, which is exactly what it also
    returns when there is simply nothing to do. So the daemon carried on looping
    forever, answering nothing, while `systemctl status` said `active` and the
    journal stayed silent -- and searches came back with cars on the map and no
    offers, which reads as broken dispatch on the rider's phone.

    Measured 2026-08-18: running `finish` cut the daemon's legs out from under
    it and neither one noticed for sixteen minutes.
    """
    def __bool__(self):
        # Falsy, so `if ride:` and `if req:` keep meaning "there is work".
        return False


EXPIRED = _Expired()


def my_active_ride(token):
    """A ride this driver is on right now, whenever it was assigned.

    Asked at the top of every loop rather than only just after accepting.
    **Accepting an offer does not create a ride** -- the rider does that when
    they tap it, which takes as long as a person takes. Waiting a fixed few
    seconds after accepting meant walking away from rides that appeared a minute
    later, leaving a real rider stuck on screen 11 with nobody driving.

    Still time-bounded, because an abandoned ride can sit in NEW indefinitely
    and there is one from a previous session in this database. Half an hour is
    far longer than any rider takes to choose and far shorter than a fossil.
    """
    r, code, _ = call("GET", f"{DRIVER_API}/ui/driver/ride/list?limit=5&offset=0",
                      None, token)
    if code == 401:
        return EXPIRED
    if code != 200 or not r:
        return None
    cutoff = datetime.now(timezone.utc) - timedelta(seconds=RECENT_RIDE_S)
    for item in r.get("list", []):
        if item.get("status") not in ("NEW", "INPROGRESS"):
            continue
        try:
            made = datetime.fromisoformat(item["createdAt"].replace("Z", "+00:00"))
        except Exception:
            continue
        if made >= cutoff:
            return item
    return None


def accept(token, req):
    """Offer to take this request. Does NOT create a ride -- the rider does."""
    sid = req.get("searchRequestId") or req.get("id")
    # Recorded before the call, not after: if the offer succeeds and the reply
    # is lost, offering again is what produces FOUND_ACTIVE_QUOTES.
    _offered.add((token, sid))
    say(f"offering on {sid[:8]} -- {req.get('distance', 0)/1000:.1f} km, "
        f"base {req.get('baseFare')} DZD", 1)
    # Omitting offeredFare accepts at base fare. It is the EXTRA on top, capped
    # at driverMaxExtraFee -- sending the total gives EXTRA_FEE_NOT_ALLOWED.
    _, code, raw = call("POST", f"{DRIVER_API}/ui/driver/searchRequest/quote/respond",
                        {"searchRequestId": sid, "response": "Accept"}, token)
    if code != 200:
        say(f"offer refused: {raw[:160]}", 2)
        return False
    say("offered. The ride appears if and when the rider picks it.", 2)
    return True


def run_ride(number, token, ride, speed):
    """Drive a ride that already exists, from wherever the driver is."""
    rid = ride["id"]
    pick = (ride["fromLocation"]["lat"], ride["fromLocation"]["lon"])
    drop = (ride["toLocation"]["lat"], ride["toLocation"]["lon"])
    say(f"driving ride {ride.get('shortRideId')} ({ride.get('status')})", 1)

    # Where the driver actually is, read back rather than assumed: this ride may
    # have been picked up on a later pass, long after the offer was made.
    here = current_position(number) or pick

    # An INPROGRESS ride is already past both of these, and calling them again
    # fails -- `start` on a started ride is not 200, which used to abandon the
    # ride here and return False. That is what a daemon restart mid-trip does,
    # so the ride it was driving became a ghost nobody would ever finish.
    if ride.get("status") == "NEW":
        drive(token, here, pick, speed, "to the pickup")

        _, code, raw = call("POST", f"{DRIVER_API}/ui/driver/ride/{rid}/arrived/pickup",
                            {"lat": pick[0], "lon": pick[1]}, token)
        say(f"arrived at pickup ({code})", 2)

        code_otp = ride_otp(rid)
        _, code, raw = call("POST", f"{DRIVER_API}/ui/driver/ride/{rid}/start",
                            {"rideOtp": code_otp,
                             "point": {"lat": pick[0], "lon": pick[1]}}, token)
        if code != 200:
            say(f"could not start: {raw[:160]}", 2)
            return False
        say(f"started with the passenger's code {code_otp}", 2)
        drive(token, pick, drop, speed, "to the destination")
    else:
        say("already in progress -- resuming from here", 2)
        drive(token, here, drop, speed, "to the destination")

    _, code, raw = call("POST", f"{DRIVER_API}/ui/driver/ride/{rid}/end",
                        {"point": {"lat": drop[0], "lon": drop[1]}}, token)
    if code != 200:
        say(f"could not end: {raw[:160]}", 2)
        return False
    fare = pg(f"SELECT fare FROM atlas_driver_offer_bpp.ride WHERE id='{rid}';")
    say(f"finished -- {fare or '?'} DZD", 2)
    return True


# ─────────────────────────────────────────────────────────────────── modes
def online(number):
    tok, _ = login(number)
    post_position(tok, BASE[[n for n, *_ in FLEET].index(number) % len(BASE)])
    _, code, raw = call("POST", f"{DRIVER_API}/ui/driver/setActivity?active=true",
                        None, tok)
    if code != 200:
        die(f"{number} could not go online: {raw[:160]}")
    return tok


def offline(number, token):
    call("POST", f"{DRIVER_API}/ui/driver/setActivity?active=false", None, token)


# Requests this driver has already answered, either way. Keyed by token so two
# drivers can each answer the same request -- which is the point of two per row.
_declined = set()
_offered = set()


def poll(token):
    """Next request this driver has not already answered.

    A request KEEPS APPEARING in nearbyRideRequest after it has been answered --
    both when declined and when offered on. Without this filter the loop
    declines a request and immediately tries to accept the same one
    (QUOTE_ALREADY_REJECTED), or offers on it again every two seconds
    (FOUND_ACTIVE_QUOTES) for the full five minutes of the search.
    """
    r, code, _ = call("GET", f"{DRIVER_API}/ui/driver/nearbyRideRequest", None, token)
    if code == 401:
        return EXPIRED
    if code != 200 or not r:
        return None
    for req in r.get("searchRequestsForDriver", []):
        sid = req.get("searchRequestId") or req.get("id")
        if (token, sid) not in _declined and (token, sid) not in _offered:
            return req
    return None


def decline(token, req, label):
    sid = req.get("searchRequestId") or req.get("id")
    _declined.add((token, sid))
    _, code, raw = call("POST", f"{DRIVER_API}/ui/driver/searchRequest/quote/respond",
                        {"searchRequestId": sid, "response": "Reject"}, token)
    say(f"{label}: declined {sid[:8]} on purpose ({code})", 1)


def cmd_finish(args):
    """Drive every unfinished ride to COMPLETED, however old it is.

    ── Why this had to exist ───────────────────────────────────────────────
    A booking that is still open BLOCKS THE RIDER ENTIRELY. Confirming any new
    quote while one is active answers `E400 INVALID_REQUEST: ACTIVE_BOOKING_
    PRESENT`, so a single ride nobody finished takes that account out of the
    product until someone clears it by hand.

    That is not hypothetical. A test booking from 10 August sat in TRIP_ASSIGNED
    for eight days; the rider tapped five different drivers on 18 August and
    every tap was refused. Screen 10 swallowed the error, so it looked exactly
    like a dead button and cost most of a morning to trace to the server.

    The daemon will never clear these: `my_active_ride` deliberately ignores
    anything older than RECENT_RIDE_S so a fossil cannot hijack a live session.
    That is the right call there and the reason this mode is separate.

    Completing rather than cancelling, deliberately -- it exercises the real
    end-of-ride path, leaves a finished trip in the rider's history, and gives
    screen 14 something to rate. `--speed 0` teleports, so it costs seconds.

    ── This STEALS the daemon's sessions ───────────────────────────────────
    Logging in as a driver revokes that driver's other token, and `movin-fleet`
    is holding one. Running this used to leave the daemon alive, `active`, and
    answering nothing at all -- for sixteen minutes, before anyone noticed.
    The daemon now recognises a revoked session and signs back in, so this is
    survivable; it is still the reason that recovery exists.
    """
    cleared = failed = 0
    for num, variant, name, *_ in FLEET:
        if not driver_id(num):
            continue
        tok, _ = login(num)
        # Deliberately NOT my_active_ride: its age cutoff is what hides these.
        r, code, _ = call("GET", f"{DRIVER_API}/ui/driver/ride/list?limit=20&offset=0",
                          None, tok)
        if code != 200 or not r:
            continue
        stuck = [x for x in r.get("list", []) if x.get("status") in ("NEW", "INPROGRESS")]
        if not stuck:
            continue
        for ride in stuck:
            say(f"{name} ({variant}) is holding {ride.get('shortRideId')} "
                f"[{ride.get('status')}] from {str(ride.get('createdAt'))[:16]}")
            if run_ride(num, tok, ride, args.speed):
                cleared += 1
            else:
                failed += 1

    say(f"\n{cleared} ride(s) finished, {failed} could not be")
    if cleared:
        say("those riders can book again -- ACTIVE_BOOKING_PRESENT is gone")
    return 0 if failed == 0 else 1


def cmd_once(args):
    wanted = [f for f in FLEET if args.variant in ("any", f[1])]
    if not wanted:
        die(f"unknown variant {args.variant}")
    for num, variant, *_ in wanted:
        if not driver_id(num):
            die(f"no driver for {variant} -- run ./simulate-driver.py seed")

    toks = {}
    for num, variant, *_ in wanted:
        toks[num] = online(num)
        say(f"{variant} driver {num} is online")
    say(f"waiting up to {args.wait}s for a request "
        f"(publish one from the app, or with Commander)")

    declined = 0
    try:
        deadline = time.time() + args.wait
        while time.time() < deadline:
            for num, variant, *_ in wanted:
                # A ride I already have beats a new request every time. Offering
                # does not create one -- the rider does, when they tap it -- so
                # this is the only reliable way to notice it happened.
                ride = my_active_ride(toks[num])
                if ride:
                    say(f"{variant} driver has a ride")
                    ok = run_ride(num, toks[num], ride, args.speed)
                    return 0 if ok else 1

                req = poll(toks[num])
                if not req:
                    continue
                if declined < args.decline:
                    declined += 1
                    decline(toks[num], req, f"{variant} ({declined}/{args.decline})")
                    continue
                accept(toks[num], req)
            time.sleep(2)
        say("nothing arrived, or nobody chose this driver's offer.")
        return 1
    finally:
        for num in toks:
            offline(num, toks[num])
        say("drivers offline")


def cmd_daemon(args):
    for num, variant, *_ in FLEET:
        if not driver_id(num):
            die(f"no driver for {variant} -- run ./simulate-driver.py seed")
    toks = {num: online(num) for num, *_ in FLEET}
    say(f"{len(toks)} drivers online, one per row the app sells. Ctrl-C to stop.")

    last_beat = time.time()
    rides = 0
    try:
        while True:
            for i, (num, variant, *_) in enumerate(FLEET):
                # Ride first, request second. Offering on a request does not
                # create a ride: the RIDER creates it by tapping that offer,
                # which takes as long as a person takes. Anything that assumed
                # the two happen together walked away from real rides and left
                # real riders waiting on screen 11.
                ride = my_active_ride(toks[num])

                # Someone else logged in as this driver and took the session
                # with them. Silently answering nothing forever is the one
                # outcome worth any amount of code to avoid, so take it back.
                if ride is EXPIRED:
                    say(f"{variant} driver's session was revoked -- signing in again")
                    toks[num] = online(num)
                    continue

                if ride:
                    say(f"{variant} driver has a ride")
                    if run_ride(num, toks[num], ride, args.speed):
                        rides += 1
                        say(f"{rides} ride(s) completed")
                    # He may have been left mid-state; put him back on duty.
                    call("POST", f"{DRIVER_API}/ui/driver/setActivity?active=true",
                         None, toks[num])
                    post_position(toks[num], BASE[i % len(BASE)])
                    continue

                req = poll(toks[num])
                if req is EXPIRED:
                    say(f"{variant} driver's session was revoked -- signing in again")
                    toks[num] = online(num)
                    continue
                if req:
                    accept(toks[num], req)
            # An idle driver whose position goes stale drops out of the pool
            # silently -- searches then return zero estimates with no error.
            if time.time() - last_beat > 30:
                for i, (num, *_) in enumerate(FLEET):
                    post_position(toks[num], BASE[i % len(BASE)])
                last_beat = time.time()
            time.sleep(2)
    except KeyboardInterrupt:
        say("stopping")
        return 0
    finally:
        for num in toks:
            offline(num, toks[num])
        say("drivers offline")


def _term(_signum, _frame):
    """Without this, `timeout`, systemd or docker stop kill the process
    outright, the cleanup never runs, and the drivers are left ONLINE with
    positions that then go stale -- which makes searches return zero estimates
    with no error anywhere. Turn the signal into the exception the modes
    already handle."""
    raise KeyboardInterrupt


def main():
    signal.signal(signal.SIGTERM, _term)
    signal.signal(signal.SIGHUP, _term)
    p = argparse.ArgumentParser(
        description="Play a driver against the live backend.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="Run this on the server: the driver API is loopback-only.")
    p.add_argument("mode", choices=["seed", "status", "once", "daemon", "finish"],
                   nargs="?", default="once")
    p.add_argument("--speed", type=float, default=8.0,
                   help="multiplier on real driving time; 1 = real time, "
                        "0 = teleport (default 8)")
    p.add_argument("--variant", default="any",
                   choices=["any", "HATCHBACK", "SEDAN", "SUV"],
                   help="which row to serve, for `once` (default any)")
    p.add_argument("--decline", type=int, default=0, metavar="N",
                   help="decline the first N requests, to exercise that path")
    p.add_argument("--wait", type=int, default=300, metavar="SEC",
                   help="how long `once` waits for a request (default 300, "
                        "the same as the backend's search expiry)")
    args = p.parse_args()

    try:
        if args.mode == "seed":
            seed(); return 0
        if args.mode == "status":
            status(); return 0
        if args.mode == "finish":
            return cmd_finish(args)
        if args.mode == "once":
            return cmd_once(args)
        return cmd_daemon(args)
    except KeyboardInterrupt:
        # Raised by Ctrl-C or by _term. The modes' `finally` has already put the
        # drivers offline by the time this is reached; this only stops the
        # traceback.
        say("interrupted")
        return 130


if __name__ == "__main__":
    sys.exit(main())
