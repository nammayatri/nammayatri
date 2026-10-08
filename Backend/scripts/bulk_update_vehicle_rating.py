#!/usr/bin/env python3
"""Bulk vehicle quality rating update (+ optional tier enrollment).

Reads rows from a CSV (columns: registration_no, rating, remark, optional
driver_id) and calls the BPP management endpoint
POST /bpp/driver-offer/{merchantShortId}/{city}/driverVehicleQuality/updateVehicleRating
one vehicle per request (the API has no bulk variant), with --workers threads
in parallel (default 10).

NOTE: the API is keyed by registration number, NOT driver id. If your source
list is driver ids, join against atlas_driver_offer_bpp.vehicle first:
  select driver_id, registration_no from atlas_driver_offer_bpp.vehicle
  where driver_id in (...);

WARNING: the server returns HTTP 200 even when registration_no matches no RC
and no vehicle (silent no-op), and every successful vehicle update recomputes
the driver's selected service tiers. Use --verify to confirm the rating
actually landed via GET /driverVehicleQuality/search?vehicleNumber=...

ENROLLMENT (interim until rating-gated auto-enrollment is deployed): the
server-side recompute is remove-only, so a rating update never ADDS a tier the
driver doesn't already hold. Pass --enroll-tiers COMFY to also call
POST /driver/{driverId}/vehicle/appendSelectedServiceTiers after each
successful update. driverId comes from an optional driver_id CSV column, or is
resolved automatically via /search. Combine with --enroll-min-rating to only
enroll rows meeting the tier's vehicle_rating threshold. NOTE: append does NOT
check usage restrictions server-side — the script-side min-rating guard is the
only gate.

RESUME: save each run's output (append `2>&1 | tee run1.log`). To re-run only
the rows that failed or were never attempted, pass the previous log(s) via
--skip-log run1.log — rows whose enrollment succeeded (or, without
--enroll-tiers, whose update verified OK) are skipped; everything else is
retried and failures land in failed_vehicle_ratings.csv as usual.

Uses only the Python standard library.

Examples:

  # Test mode: only these vehicles, CSV ignored (rating/remark from flags)
  python3 bulk_update_vehicle_rating.py \
      --host https://dashboard.example.com \
      --merchant-short-id NAMMA_YATRI_PARTNER --city Bangalore \
      --token "$DASHBOARD_TOKEN" \
      --test-registration-nos "KA01AB1234,KA02CD5678" \
      --rating 4.5 --remark "QC inspection Oct 2026"

  # Dry run against the full CSV (prints rows, no API calls)
  python3 bulk_update_vehicle_rating.py --host ... --merchant-short-id ... \
      --city ... --token "$DASHBOARD_TOKEN" --csv ratings.csv --dry-run

  # Full run: rate AND enroll qualifying vehicles, 10 threads, log saved
  python3 bulk_update_vehicle_rating.py --host ... --merchant-short-id ... \
      --city ... --token "$DASHBOARD_TOKEN" --csv ratings.csv --verify \
      --enroll-tiers COMFY --enroll-min-rating 4.5 2>&1 | tee run1.log

  # Resume: skip rows already enrolled in run1.log, retry the rest
  python3 bulk_update_vehicle_rating.py --host ... --merchant-short-id ... \
      --city ... --token "$DASHBOARD_TOKEN" --csv ratings.csv --verify \
      --enroll-tiers COMFY --enroll-min-rating 4.5 \
      --skip-log run1.log 2>&1 | tee run2.log
"""

import argparse
import concurrent.futures
import csv
import json
import re
import sys
import threading
import time
import urllib.error
import urllib.parse
import urllib.request

FAILED_CSV = "failed_vehicle_ratings.csv"

print_lock = threading.Lock()


class Pacer:
    """Global pacing across all workers: at most `per_minute` request starts/min.

    The server enforces dashboardApiRateLimitOptions (a sliding window per
    operator personId, e.g. 300 hits / 60s) on EVERY direct-dashboard call at
    the auth layer, so update+search+append all count against one budget.
    """

    def __init__(self, per_minute):
        self.interval = 60.0 / per_minute if per_minute else 0.0
        self.lock = threading.Lock()
        self.next_t = 0.0

    def wait(self):
        if not self.interval:
            return
        with self.lock:
            now = time.monotonic()
            slot = max(now, self.next_t)
            self.next_t = slot + self.interval
        delay = slot - time.monotonic()
        if delay > 0:
            time.sleep(delay)


PACER = Pacer(0)


def say(msg):
    with print_lock:
        print(msg, flush=True)


def parse_args():
    p = argparse.ArgumentParser(description="Bulk vehicle quality rating update")
    p.add_argument("--host", required=True, help="Dashboard base URL, e.g. https://dashboard.example.com")
    p.add_argument("--merchant-short-id", required=True, help="Merchant short id, e.g. NAMMA_YATRI_PARTNER")
    p.add_argument("--city", required=True, help="Operating city as used in dashboard URLs, e.g. Bangalore")
    p.add_argument("--token", required=True, help="Dashboard operator session token (sent as 'token' header)")
    p.add_argument("--csv", default=None,
                   help="CSV with columns: registration_no, rating, remark, optional driver_id "
                        "(rating/remark optional per row if --rating/--remark given)")
    p.add_argument("--test-registration-nos", default=None,
                   help="TEST MODE: comma-separated registration numbers; CSV is ignored")
    p.add_argument("--rating", type=float, default=None,
                   help="Default rating (1-5) for rows without a rating column/value")
    p.add_argument("--remark", default=None,
                   help="Default remark for rows without a remark column/value")
    p.add_argument("--workers", type=int, default=10,
                   help="Concurrent rows in flight (default: %(default)s); each row is up to 3 API calls")
    p.add_argument("--rate-limit", type=int, default=280,
                   help="Max API calls per minute across ALL workers (default: %(default)s). The server "
                        "allows dashboardApiRateLimitOptions hits/min per operator (300/60s in dev) "
                        "counting update+search+append together; stay below it. 0 disables pacing.")
    p.add_argument("--enroll-tiers", default=None,
                   help="Comma-separated service tiers (e.g. COMFY) to append to the driver's "
                        "selected tiers after each successful rating update, via "
                        "/driver/{driverId}/vehicle/appendSelectedServiceTiers")
    p.add_argument("--enroll-min-rating", type=float, default=None,
                   help="Only enroll rows whose rating is >= this (set it to the tier's "
                        "vehicle_rating threshold, e.g. 4.5); lower-rated rows are rated but not enrolled")
    p.add_argument("--skip-log", action="append", default=[],
                   help="Previous run log file; rows that fully succeeded in it are skipped. "
                        "Repeatable for multiple logs.")
    p.add_argument("--verify", action="store_true",
                   help="After each update, GET /search?vehicleNumber= and confirm the rating landed "
                        "(catches the server's silent no-op on unknown registration numbers)")
    p.add_argument("--per-call-sleep", type=float, default=0.0,
                   help="Seconds each worker sleeps between rows (default: %(default)s)")
    p.add_argument("--retries", type=int, default=2, help="Retries per failed update call (default: %(default)s)")
    p.add_argument("--dry-run", action="store_true", help="Print rows without calling the API")
    p.add_argument("--start-row", type=int, default=1,
                   help="1-indexed row (after --skip-log filtering) to start from (default: %(default)s)")
    return p.parse_args()


def load_rows(args):
    if args.test_registration_nos:
        if args.rating is None or args.remark is None:
            sys.exit("ERROR: --test-registration-nos requires --rating and --remark")
        regs = [r.strip() for r in args.test_registration_nos.split(",") if r.strip()]
        print(f"TEST MODE: using {len(regs)} vehicle(s) from --test-registration-nos, CSV ignored")
        return [{"registration_no": r, "rating": args.rating, "remark": args.remark, "driver_id": None} for r in regs]

    if not args.csv:
        sys.exit("ERROR: provide --csv or --test-registration-nos")

    with open(args.csv, newline="") as f:
        reader = csv.DictReader(f)
        cols = reader.fieldnames or []
        if "registration_no" not in cols:
            sys.exit(f"ERROR: column 'registration_no' not found in {args.csv} (columns: {cols}). "
                     "This API is keyed by registration number, not driver id — "
                     "join driver ids against atlas_driver_offer_bpp.vehicle to get it.")
        rows, bad = [], []
        for i, row in enumerate(reader, start=2):  # line 1 is the header
            reg = (row.get("registration_no") or "").strip()
            if not reg:
                continue
            rating_raw = (row.get("rating") or "").strip()
            remark = (row.get("remark") or "").strip() or args.remark
            try:
                rating = float(rating_raw) if rating_raw else args.rating
            except ValueError:
                rating = None
            if rating is None or not (1 <= rating <= 5) or not remark:
                bad.append((i, reg, rating_raw or args.rating, remark))
                continue
            rows.append({"registration_no": reg, "rating": rating, "remark": remark,
                         "driver_id": (row.get("driver_id") or "").strip() or None})

    if bad:
        for line_no, reg, rating, remark in bad[:10]:
            print(f"  line {line_no}: {reg!r} rating={rating!r} remark={remark!r}")
        sys.exit(f"ERROR: {len(bad)} row(s) invalid (rating must be 1-5 and remark non-empty; "
                 "fix the CSV or pass --rating/--remark defaults)")

    # de-duplicate by registration_no, keep last occurrence (later rows override)
    by_reg = {r["registration_no"]: r for r in rows}
    dupes = len(rows) - len(by_reg)
    unique = list(by_reg.values())
    print(f"Loaded {len(unique)} unique vehicle(s) from {args.csv}"
          + (f" ({dupes} duplicate registration_no dropped, last value kept)" if dupes else ""))
    return unique


def load_done_regs(log_paths, enrolling):
    """Registration numbers that fully succeeded in previous run logs.

    With enrollment on, success means an 'enrolled' or 'enroll skipped' line;
    otherwise an 'OK (verified)' line. Failed/suspect/never-attempted rows are
    absent from the set and get retried.
    """
    done = set()
    pat_enrolled = re.compile(r"\] (\S+) (?:enrolled |enroll skipped)")
    pat_ok = re.compile(r"\] (\S+) OK \(verified\)")
    pat = pat_enrolled if enrolling else pat_ok
    for path in log_paths:
        with open(path) as f:
            for line in f:
                m = pat.search(line)
                if m:
                    done.add(m.group(1))
    return done


def request_json(url, token, payload=None):
    req = urllib.request.Request(
        url,
        data=json.dumps(payload).encode("utf-8") if payload is not None else None,
        headers={"Content-Type": "application/json", "token": token},
        method="POST" if payload is not None else "GET",
    )
    # HITS_LIMIT_EXCEED (429) is the per-operator sliding window; waiting out
    # the window and retrying is always correct, so handle it here for all
    # three call types uniformly instead of burning the caller's retries.
    for _ in range(5):
        PACER.wait()
        try:
            with urllib.request.urlopen(req, timeout=60) as resp:
                return resp.status, resp.read().decode("utf-8", errors="replace")
        except urllib.error.HTTPError as e:
            if e.code != 429:
                raise
            body = e.read().decode("utf-8", errors="replace")
            m = re.search(r"in (\d+) sec", body)
            wait_s = int(m.group(1)) + 1 if m else 61
            say(f"rate-limited (429), all workers pausing up to {wait_s}s")
            time.sleep(wait_s)
    raise OSError("still rate-limited after 5 waits")


def search_vehicle(base_url, token, registration_no):
    """GET /search?vehicleNumber= — first result dict, or None if absent/unreachable."""
    url = f"{base_url}/search?vehicleNumber={urllib.parse.quote(registration_no)}"
    try:
        status, body = request_json(url, token)
        results = json.loads(body) if 200 <= status < 300 else []
    except (urllib.error.URLError, TimeoutError, OSError, ValueError):
        return None
    return results[0] if results else None


def check_rating(search_result, row):
    """Confirm the rating actually landed (HTTP 200 on update != updated)."""
    if search_result is None:
        return "not-found"
    got = search_result.get("vehicleRating")
    return "ok" if got is not None and abs(got - row["rating"]) < 1e-9 else f"mismatch (server has {got})"


def enroll_driver(root_url, token, driver_id, tiers, label):
    """Append tiers to the driver's selection; returns (ok, failure_reason)."""
    url = f"{root_url}/driver/{urllib.parse.quote(driver_id)}/vehicle/appendSelectedServiceTiers"
    try:
        status, body = request_json(url, token, {"selected_service_tiers": tiers})
        if 200 <= status < 300:
            say(f"{label} enrolled {','.join(tiers)} for driver {driver_id}")
            return True, None
        say(f"{label} enroll HTTP {status} {body[:200]}")
        reason = f"enroll HTTP {status}: {body[:300]}"
    except urllib.error.HTTPError as e:
        body = e.read().decode("utf-8", errors="replace")
        say(f"{label} enroll HTTP {e.code} {body[:300]}")
        reason = f"enroll HTTP {e.code}: {body[:300]}"
        if "VEHICLE_DOES_NOT_EXIST" in body:
            reason = ("rating updated, enroll skipped: driver " + driver_id
                      + " has no active vehicle row (vehicle re-assigned / driver changed) — " + reason)
    except (urllib.error.URLError, TimeoutError, OSError) as e:
        say(f"{label} enroll network error: {e}")
        reason = f"enroll network error: {e}"
    return False, reason


def process_row(args, urls, enroll_tiers, row, label):
    """Full lifecycle for one row. Returns ('ok'|'failed'|'suspect', failure_reason)."""
    root_url, base_url, update_url = urls
    if args.per_call_sleep:
        time.sleep(args.per_call_sleep)
    payload = {"registrationNo": row["registration_no"], "rating": row["rating"], "remark": row["remark"]}
    will_enroll = enroll_tiers and (args.enroll_min_rating is None or row["rating"] >= args.enroll_min_rating)

    success, reason = False, None
    for attempt in range(1, args.retries + 2):
        try:
            status, body = request_json(update_url, args.token, payload)
            if 200 <= status < 300:
                success = True
                break
            say(f"{label} attempt {attempt}: HTTP {status} {body[:200]}")
            reason = f"update HTTP {status}: {body[:300]}"
        except urllib.error.HTTPError as e:
            body = e.read().decode("utf-8", errors="replace")
            say(f"{label} attempt {attempt}: HTTP {e.code} {body[:300]}")
            reason = f"update HTTP {e.code}: {body[:300]}"
            if e.code in (400, 401, 403):
                break  # not retryable: bad request / auth — fix and re-run
        except (urllib.error.URLError, TimeoutError, OSError) as e:
            say(f"{label} attempt {attempt}: network error: {e}")
            reason = f"update network error: {e}"
        if attempt <= args.retries:
            time.sleep(2 * attempt)

    if not success:
        return "failed", reason

    need_search = args.verify or (will_enroll and not row.get("driver_id"))
    found = search_vehicle(base_url, args.token, row["registration_no"]) if need_search else None
    if args.verify:
        outcome = check_rating(found, row)
        if outcome == "not-found":
            say(f"{label} HTTP 200 but verification failed: not-found "
                "— likely unknown registration number (server no-ops silently)")
            return "suspect", ("nothing updated: registration number has no active vehicle "
                               "(update returned 200 but /search found nothing — server no-ops silently)")
        if outcome != "ok":
            say(f"{label} HTTP 200 but verification failed: {outcome}")
            return "suspect", f"rating verification failed: {outcome}"
        say(f"{label} OK (verified)")
    else:
        say(f"{label} OK (unverified — 200 does not guarantee an update)")

    if will_enroll:
        driver_id = row.get("driver_id") or (found or {}).get("driverId")
        if not driver_id:
            say(f"{label} enroll failed: could not resolve driverId "
                "(no driver_id column and /search returned nothing)")
            return "suspect", "rating updated, enroll skipped: could not resolve driverId from /search"
        enrolled, enroll_reason = enroll_driver(root_url, args.token, driver_id, enroll_tiers, label)
        if not enrolled:
            return "suspect", enroll_reason
    elif enroll_tiers:
        say(f"{label} enroll skipped: rating {row['rating']} < {args.enroll_min_rating}")
    return "ok", None


def main():
    global PACER
    args = parse_args()
    PACER = Pacer(args.rate_limit)
    rows = load_rows(args)
    enroll_tiers = [t.strip() for t in args.enroll_tiers.split(",") if t.strip()] if args.enroll_tiers else []

    if args.skip_log:
        done = load_done_regs(args.skip_log, bool(enroll_tiers))
        before = len(rows)
        rows = [r for r in rows if r["registration_no"] not in done]
        print(f"Skip-log      : {before - len(rows)} already-succeeded row(s) skipped "
              f"({len(done)} successes found in {len(args.skip_log)} log(s)); {len(rows)} to process")
    if args.start_row > 1:
        rows = rows[args.start_row - 1:]
    if not rows:
        sys.exit("ERROR: no vehicles to process")

    root_url = f"{args.host.rstrip('/')}/bpp/driver-offer/{args.merchant_short_id}/{args.city}"
    base_url = f"{root_url}/driverVehicleQuality"
    update_url = f"{base_url}/updateVehicleRating"
    urls = (root_url, base_url, update_url)
    total = len(rows)

    print(f"Endpoint      : {update_url}")
    if enroll_tiers:
        print(f"Enrollment    : appending {enroll_tiers} via {root_url}/driver/{{driverId}}/vehicle/appendSelectedServiceTiers"
              + (f" for rows with rating >= {args.enroll_min_rating}" if args.enroll_min_rating is not None else " for ALL rows (no --enroll-min-rating guard!)"))
    print(f"Vehicles      : {total}, {args.workers} worker(s) in parallel, "
          f"paced at {args.rate_limit or 'unlimited'} API calls/min"
          + (f" (~{args.rate_limit // 3} rows/min with verify+enroll)" if args.rate_limit else ""))
    print("NOTE          : one update call per vehicle; each update recomputes the driver's service tiers")
    if args.dry_run:
        print("DRY RUN — no API calls will be made\n")
        for i, row in enumerate(rows, start=1):
            will_enroll = enroll_tiers and (args.enroll_min_rating is None or row["rating"] >= args.enroll_min_rating)
            payload = {"registrationNo": row["registration_no"], "rating": row["rating"], "remark": row["remark"]}
            print(f"[{i}/{total}] {row['registration_no']} DRY RUN — {json.dumps(payload)}"
                  + (f" + enroll {enroll_tiers}" if will_enroll else ""))
        print(f"\nDry run complete: {total} vehicle(s)")
        return

    ok_count = 0
    failed, suspect = [], []
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.workers) as pool:
        futures = {
            pool.submit(process_row, args, urls, enroll_tiers, row, f"[{i}/{total}] {row['registration_no']}"): row
            for i, row in enumerate(rows, start=1)
        }
        try:
            for fut in concurrent.futures.as_completed(futures):
                row = futures[fut]
                try:
                    status, reason = fut.result()
                except Exception as e:  # defensive: a worker bug must not kill the run
                    say(f"[?] {row['registration_no']} worker error: {e}")
                    status, reason = "failed", f"worker error: {e}"
                if status == "ok":
                    ok_count += 1
                else:
                    (failed if status == "failed" else suspect).append({**row, "failure_reason": reason or ""})
        except KeyboardInterrupt:
            say("\nInterrupted — waiting for in-flight rows, unstarted rows cancelled. "
                "Re-run with --skip-log <this run's log> to resume.")
            pool.shutdown(wait=False, cancel_futures=True)
            raise SystemExit(130)

    print(f"\nDone. Succeeded: {ok_count}; failed: {len(failed)}; unverified/suspect: {len(suspect)}")
    leftovers = failed + suspect
    if leftovers:
        with open(FAILED_CSV, "w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=["registration_no", "rating", "remark", "driver_id", "failure_reason"])
            writer.writeheader()
            writer.writerows(leftovers)
        print(f"Failed/suspect rows written to {FAILED_CSV} — re-run with --csv {FAILED_CSV}")
        sys.exit(1)


if __name__ == "__main__":
    main()
