#!/usr/bin/env python3
"""Bulk fee waive-off for driver subscription.

Reads driver ids from a CSV (column: driver_id) and calls the provider-dashboard
endpoint POST /bpp/driver-offer/{merchantShortId}/{city}/plan/waiveOff/fee in
batches of --batch-size (must be <= transporterConfig.bulkWaiveOffLimit,
default 200 on the server).

Uses only the Python standard library.

Examples:

  # Test mode: only these driver ids, everything else ignored
  python3 bulk_waive_off.py \
      --host https://dashboard.example.com \
      --merchant-short-id NAMMA_YATRI_PARTNER --city Bangalore \
      --token "$DASHBOARD_TOKEN" \
      --test-driver-ids "id-1,id-2"

  # Dry run against the full CSV (prints batches, no API calls)
  python3 bulk_waive_off.py --host ... --merchant-short-id ... --city ... \
      --token "$DASHBOARD_TOKEN" --dry-run

  # Full run
  python3 bulk_waive_off.py --host ... --merchant-short-id ... --city ... \
      --token "$DASHBOARD_TOKEN"

  # Re-run only the batches that failed last time
  python3 bulk_waive_off.py --host ... --merchant-short-id ... --city ... \
      --token "$DASHBOARD_TOKEN" --csv failed_driver_ids.csv
"""

import argparse
import csv
import json
import sys
import time
import urllib.error
import urllib.request

DEFAULT_CSV = "/Users/hemantmangla/Downloads/new_activation - final_list.c"
FAILED_CSV = "failed_driver_ids.csv"


def parse_args():
    p = argparse.ArgumentParser(description="Bulk subscription fee waive-off")
    p.add_argument("--host", required=True, help="Dashboard base URL, e.g. https://dashboard.example.com")
    p.add_argument("--merchant-short-id", required=True, help="Merchant short id, e.g. NAMMA_YATRI_PARTNER")
    p.add_argument("--city", required=True, help="Operating city as used in dashboard URLs, e.g. Bangalore")
    p.add_argument("--token", required=True, help="Dashboard auth token (sent as 'token' header)")
    p.add_argument("--csv", default=DEFAULT_CSV, help="CSV with a driver_id column (default: %(default)s)")
    p.add_argument("--test-driver-ids", default=None,
                   help="TEST MODE: comma-separated driver ids; CSV is ignored")
    p.add_argument("--batch-size", type=int, default=200,
                   help="Drivers per API call; must be <= server bulkWaiveOffLimit (default: %(default)s)")
    p.add_argument("--percentage", type=float, default=100.0, help="Waive-off percentage (default: %(default)s)")
    p.add_argument("--days-valid-for", type=int, default=7, help="Validity window in days (default: %(default)s)")
    p.add_argument("--waive-off-mode", default="WITHOUT_OFFER",
                   choices=["WITH_OFFER", "WITHOUT_OFFER", "NO_WAIVE_OFF"],
                   help="Waive-off mode (default: %(default)s)")
    p.add_argument("--service-name", default="YATRI_SUBSCRIPTION",
                   help="Subscription service name (default: %(default)s)")
    p.add_argument("--sleep", type=float, default=1.0, help="Seconds between batches (default: %(default)s)")
    p.add_argument("--retries", type=int, default=2, help="Retries per failed batch (default: %(default)s)")
    p.add_argument("--dry-run", action="store_true", help="Print batches without calling the API")
    p.add_argument("--start-batch", type=int, default=1,
                   help="1-indexed batch to start from, to resume a partial run (default: %(default)s)")
    return p.parse_args()


def load_driver_ids(args):
    if args.test_driver_ids:
        ids = [d.strip() for d in args.test_driver_ids.split(",") if d.strip()]
        print(f"TEST MODE: using {len(ids)} driver id(s) from --test-driver-ids, CSV ignored")
        return ids
    with open(args.csv, newline="") as f:
        reader = csv.DictReader(f)
        if "driver_id" not in (reader.fieldnames or []):
            sys.exit(f"ERROR: column 'driver_id' not found in {args.csv} (columns: {reader.fieldnames})")
        ids = [row["driver_id"].strip() for row in reader if row["driver_id"].strip()]
    # de-duplicate, keep order
    seen, unique = set(), []
    for d in ids:
        if d not in seen:
            seen.add(d)
            unique.append(d)
    dupes = len(ids) - len(unique)
    print(f"Loaded {len(unique)} unique driver id(s) from {args.csv}" + (f" ({dupes} duplicates dropped)" if dupes else ""))
    return unique


def build_payload(driver_ids, args):
    return {
        "waiveOffEntities": [
            {
                "driverId": driver_id,
                "percentage": args.percentage,
                # field is spelled 'waiveOfMode' (single f) in the API type
                "waiveOfMode": args.waive_off_mode,
                "daysValidFor": args.days_valid_for,
                "serviceName": args.service_name,
            }
            for driver_id in driver_ids
        ]
    }


def post_batch(url, token, payload):
    req = urllib.request.Request(
        url,
        data=json.dumps(payload).encode("utf-8"),
        headers={"Content-Type": "application/json", "token": token},
        method="POST",
    )
    with urllib.request.urlopen(req, timeout=120) as resp:
        return resp.status, resp.read().decode("utf-8", errors="replace")


def main():
    args = parse_args()
    driver_ids = load_driver_ids(args)
    if not driver_ids:
        sys.exit("ERROR: no driver ids to process")

    url = f"{args.host.rstrip('/')}/bpp/driver-offer/{args.merchant_short_id}/{args.city}/plan/waiveOff/fee"
    batches = [driver_ids[i:i + args.batch_size] for i in range(0, len(driver_ids), args.batch_size)]

    print(f"Endpoint      : {url}")
    print(f"Waive-off     : {args.percentage}% for {args.days_valid_for} day(s), "
          f"mode={args.waive_off_mode}, service={args.service_name}")
    print(f"Batches       : {len(batches)} of up to {args.batch_size} driver(s) each"
          + (f", starting at batch {args.start_batch}" if args.start_batch > 1 else ""))
    if args.dry_run:
        print("DRY RUN — no API calls will be made\n")

    failed = []
    ok_count = 0
    for i, batch in enumerate(batches, start=1):
        if i < args.start_batch:
            continue
        label = f"[batch {i}/{len(batches)}] {len(batch)} driver(s)"
        payload = build_payload(batch, args)
        if args.dry_run:
            preview = payload["waiveOffEntities"][0]
            print(f"{label} DRY RUN — first entity: {json.dumps(preview)}")
            continue

        success = False
        for attempt in range(1, args.retries + 2):
            try:
                status, body = post_batch(url, args.token, payload)
                print(f"{label} attempt {attempt}: HTTP {status} {body[:200]}")
                if 200 <= status < 300:
                    success = True
                    break
            except urllib.error.HTTPError as e:
                body = e.read().decode("utf-8", errors="replace")
                print(f"{label} attempt {attempt}: HTTP {e.code} {body[:300]}")
                if e.code in (400, 401, 403):
                    break  # not retryable: bad request / auth — fix and re-run
            except (urllib.error.URLError, TimeoutError, OSError) as e:
                print(f"{label} attempt {attempt}: network error: {e}")
            if attempt <= args.retries:
                time.sleep(2 * attempt)

        if success:
            ok_count += len(batch)
        else:
            failed.extend(batch)
        time.sleep(args.sleep)

    if args.dry_run:
        print(f"\nDry run complete: {len(batches)} batch(es), {len(driver_ids)} driver(s) total")
        return

    print(f"\nDone. Succeeded: {ok_count} driver(s); failed: {len(failed)} driver(s)")
    if failed:
        with open(FAILED_CSV, "w", newline="") as f:
            writer = csv.writer(f)
            writer.writerow(["driver_id"])
            writer.writerows([d] for d in failed)
        print(f"Failed driver ids written to {FAILED_CSV} — re-run with --csv {FAILED_CSV}")
        sys.exit(1)


if __name__ == "__main__":
    main()
