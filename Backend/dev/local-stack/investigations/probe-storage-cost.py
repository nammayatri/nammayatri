#!/usr/bin/env python3
"""What does the pilot actually consume per ride, and what fills the disk first?

The client asked how many riders, drivers and rides this server can hold. Rows
are the obvious answer and the wrong one: a rider account is 324 bytes, so
accounts are never the constraint. Three things really consume the disk, in
this order, and only the first is usually thought about:

  1. completed rides, with their protocol logs
  2. CONTAINER LOGS, which have no rotation configured anywhere in the compose
     file. Docker's json-file driver keeps everything forever by default, and
     the driver service logs every Redis call it makes.
  3. the place index and map tiles, which are fixed and already paid for

This measures 1 by driving real rides end to end and weighing the database
before and after, and 2 by watching the log files grow in real time.
"""
import json
import re
import subprocess
import sys
import time
from datetime import datetime, timezone

RIDES = 2          # each is a full search -> offer -> confirm -> start -> end
LOG_WINDOW = 90    # seconds of log growth to extrapolate from


def say(m):
    print(m, flush=True)


def sh(cmd, timeout=600):
    return subprocess.run(cmd, shell=True, capture_output=True, text=True,
                          timeout=timeout)


def pg(sql):
    out = subprocess.run(
        ["docker", "exec", "ny-postgres", "psql", "-U", "postgres", "-d", "atlas_dev",
         "-At", "-c", sql], capture_output=True, text=True, timeout=180)
    return out.stdout.strip()


def counts():
    return {
        "bytes": int(pg("SELECT pg_database_size('atlas_dev')") or 0),
        "rides": int(pg("SELECT count(*) FROM atlas_app.ride") or 0),
        "bookings": int(pg("SELECT count(*) FROM atlas_app.booking") or 0),
        "searches": int(pg("SELECT count(*) FROM atlas_app.search_request") or 0),
        "beckn": int(pg("SELECT count(*) FROM atlas_app.beckn_request") or 0),
    }


say("=" * 80)
say("STORAGE COST")
say(f"{datetime.now(timezone.utc).isoformat(timespec='seconds')}")
say("=" * 80)

# ------------------------------------------------- 2. logs, measured while we work
say("")
say("--- container logs: how fast do they grow, with the stack merely idling?")
LOGDIR = "/var/lib/docker/containers"
first = sh(f"du -sb {LOGDIR} 2>/dev/null | cut -f1").stdout.strip()
say(f"  container logs on disk right now: {int(first)/1e6:.0f} MB")
log_t0 = time.time()

# ---------------------------------------------------------- 1. real rides
say("")
say(f"--- driving {RIDES} complete rides through the real flow")
before = counts()
say(f"  before: {before['bytes']/1e6:.1f} MB, {before['rides']} rides on record")

ok = 0
for i in range(RIDES):
    t0 = time.time()
    r = sh("python3 probe-booking-flow.py", timeout=420)
    tail = [ln for ln in r.stdout.splitlines() if ln.strip()][-1:] or ["(no output)"]
    done = "COMPLETED" in r.stdout or "rate the ride" in r.stdout
    ok += 1 if done else 0
    say(f"  ride {i+1}: {time.time()-t0:>5.0f}s  {'completed' if done else 'DID NOT COMPLETE'}"
        f"   last line: {tail[0][:90]}")
    if not done:
        # Worth seeing: a ride that will not complete is usually leftover state
        # from the previous one, and that is itself the answer to a later
        # question about how the pilot behaves under repeated use.
        for ln in r.stdout.splitlines()[-6:]:
            say(f"      | {ln[:110]}")

pg("VACUUM ANALYZE")
after = counts()
d_rides = after["rides"] - before["rides"]
d_bytes = after["bytes"] - before["bytes"]
say("")
say(f"  after: {after['bytes']/1e6:.1f} MB   (+{d_rides} rides, "
    f"+{after['bookings']-before['bookings']} bookings, "
    f"+{after['searches']-before['searches']} searches, "
    f"+{after['beckn']-before['beckn']} protocol log rows)")
if d_rides > 0:
    per = d_bytes / d_rides
    say(f"  -> ONE COMPLETED RIDE COSTS {per/1024:.0f} kB of database, measured")
else:
    per = None
    say("  -> no ride completed; cannot weigh one")

# ------------------------------------------------------------- logs, second look
elapsed = time.time() - log_t0
if elapsed < LOG_WINDOW:
    time.sleep(LOG_WINDOW - elapsed)
    elapsed = time.time() - log_t0
second = sh(f"du -sb {LOGDIR} 2>/dev/null | cut -f1").stdout.strip()
grew = int(second) - int(first)
say("")
say(f"--- logs again, {elapsed:.0f}s later: grew {grew/1e6:.1f} MB")
if grew > 0:
    per_day = grew / elapsed * 86400
    say(f"  -> {per_day/1e6:.0f} MB/day, {per_day*30/1e9:.1f} GB/month "
        f"AT THIS LEVEL OF USE (two rides and some probing)")
say("  biggest log files:")
say(sh("du -sh /var/lib/docker/containers/*/ 2>/dev/null | sort -rh | head -5").stdout.rstrip())
say("  rotation configured?  " + (sh(
    "docker inspect ny-driver --format '{{json .HostConfig.LogConfig}}'").stdout.strip()))

# --------------------------------------------------------------- what is on disk
say("")
say("--- where the 24 GB already used has gone")
say(sh("du -sh /var/lib/docker/volumes/* 2>/dev/null | sort -rh | head -6").stdout.rstrip())
free_b = int(sh("df -B1 --output=avail / | tail -1").stdout.strip())
say(f"  free: {free_b/2**30:.0f} GiB")

# ------------------------------------------------------------------ the arithmetic
say("")
say("=" * 80)
say("HOW LONG THE DISK LASTS")
say("=" * 80)
say("accounts first, because they are the thing that was asked about:")
for label, tbl in [("rider", "atlas_app.person"), ("driver", "atlas_driver_offer_bpp.person")]:
    w = int(pg(f"SELECT coalesce(round(avg(pg_column_size(t.*))),0) FROM {tbl} t") or 0)
    if w:
        # x3 covers the indexes and the rows that hang off a person: their
        # session token, their device, their saved addresses.
        each = w * 3
        say(f"  one {label} account ~{each} bytes with indexes -> "
            f"{free_b/each/1e6:.0f} million of them would fit in the free space")

if per:
    say("")
    say(f"rides, at {per/1024:.0f} kB each:")
    for per_day in (100, 500, 2000, 10000):
        days = free_b / (per * per_day)
        say(f"  {per_day:>6,} rides/day -> {days:>7,.0f} days "
            f"({days/365:.1f} years) before the disk is full")
say("")
say("done " + datetime.now(timezone.utc).isoformat(timespec="seconds"))
