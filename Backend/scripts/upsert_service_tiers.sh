#!/usr/bin/env bash
# Upload driver service tiers in small chunks, in parallel, via the dashboard
# upsertSelectedServiceTiers API.
#
# Usage:
#   ./upsert_service_tiers.sh --file TAXI_PLUS.csv --token <token> --dry-run   # single-driver upload to verify the API works
#   ./upsert_service_tiers.sh --file TAXI_PLUS.csv --token <token>             # chunk the full file and upload all chunks
#   ./upsert_service_tiers.sh --file AUTO_LITE.csv --token <token>             # same for another tier
#   (token can also be passed via the DASHBOARD_TOKEN env var instead of --token)
#
# Input file: one driverId per line, header "driverId". A relative --file path is
# resolved against this script's directory. The service tier written for every
# driver is the input file's base name (e.g. TAXI_PLUS.csv -> TAXI_PLUS).
#
# Chunks are uploaded PARALLEL_JOBS at a time. Successfully uploaded chunks are
# moved to <chunks_dir>/done/; failed ones stay in place, so re-running the
# script resumes with only the chunks that haven't succeeded yet.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

CHUNK_SIZE="${CHUNK_SIZE:-200}"
PARALLEL_JOBS="${PARALLEL_JOBS:-10}"
URL="${UPSERT_URL:-https://dashboard.moving.tech/api/bpp/driver-offer/NAMMA_YATRI_PARTNER/Bangalore/driver/vehicle/upsertSelectedServiceTiers}"
TOKEN="${DASHBOARD_TOKEN:-}"
INPUT_FILE="${INPUT_FILE:-}"

DRY_RUN=false
while [[ $# -gt 0 ]]; do
  case "$1" in
    --dry-run) DRY_RUN=true; shift ;;
    --token) TOKEN="${2:-}"; shift 2 ;;
    --file) INPUT_FILE="${2:-}"; shift 2 ;;
    *) echo "ERROR: unknown argument: $1" >&2; exit 1 ;;
  esac
done

[[ -n "$TOKEN" ]] || { echo "ERROR: token required — pass --token <token> or set DASHBOARD_TOKEN" >&2; exit 1; }
[[ -n "$INPUT_FILE" ]] || { echo "ERROR: input file required — pass --file <tier>.csv (e.g. --file TAXI_PLUS.csv)" >&2; exit 1; }
# Resolve relative paths against the script directory.
[[ "$INPUT_FILE" == /* ]] || INPUT_FILE="$SCRIPT_DIR/$INPUT_FILE"
[[ -f "$INPUT_FILE" ]] || { echo "ERROR: input file not found: $INPUT_FILE" >&2; exit 1; }

SERVICE_TIER="$(basename "$INPUT_FILE" .csv)"
WORK_DIR="$(cd "$(dirname "$INPUT_FILE")" && pwd)/${SERVICE_TIER}_chunks"
DONE_DIR="$WORK_DIR/done"
FAIL_LOG="$WORK_DIR/failed_chunks.log"

upload_worker() {
  local file="$1"
  local drivers=$(($(wc -l < "$file") - 1))
  local response http_code body
  response=$(curl --silent --show-error --location "$URL" \
    --header 'accept: application/json;charset=utf-8' \
    --header "token: $TOKEN" \
    --form "file=@\"$file\"" \
    --write-out '\n%{http_code}' 2>&1) || true
  http_code=$(echo "$response" | tail -n1)
  body=$(echo "$response" | sed '$d')
  if [[ "$http_code" == "200" ]]; then
    echo "OK   $(basename "$file") ($drivers drivers)"
    mv "$file" "$DONE_DIR/"
  else
    echo "FAIL $(basename "$file") ($drivers drivers) HTTP $http_code: $body"
    echo "$file HTTP $http_code: $body" >> "$FAIL_LOG"
  fi
}
export -f upload_worker
export URL TOKEN DONE_DIR FAIL_LOG

mkdir -p "$WORK_DIR" "$DONE_DIR"

if $DRY_RUN; then
  # Build a file with a single known driver id and upload it.
  dry_driver="${DRY_RUN_DRIVER_ID:-181f1845-e3d6-4b22-a0f6-796eff9c2f24}"
  dry_file="$WORK_DIR/dry_run.csv"
  {
    echo "driverId,selectedServiceTiers"
    echo "$dry_driver,$SERVICE_TIER"
  } > "$dry_file"
  echo "DRY RUN: single driver $dry_driver with tier $SERVICE_TIER"
  rm -f "$FAIL_LOG"
  upload_worker "$dry_file"
  if [[ -f "$FAIL_LOG" ]]; then
    echo "Dry run FAILED." >&2
    exit 1
  fi
  echo "Dry run succeeded. Re-run without --dry-run to upload the full file."
  exit 0
fi

# --- Full run: chunk (if needed) and upload in parallel ---
shopt -s nullglob
pending=( "$WORK_DIR"/chunk_*.csv )
done_files=( "$DONE_DIR"/chunk_*.csv )
if (( ${#pending[@]} > 0 )); then
  echo "Found ${#pending[@]} pending chunk files in $WORK_DIR — resuming (delete them to re-chunk from scratch)."
elif (( ${#done_files[@]} > 0 )); then
  echo "All ${#done_files[@]} chunks already uploaded (see $DONE_DIR)."
  echo "To upload everything again from scratch, delete $DONE_DIR first."
  exit 0
else
  echo "Chunking $INPUT_FILE into batches of $CHUNK_SIZE (tier: $SERVICE_TIER)..."
  # Skip header, drop blank lines/whitespace, append tier column, split into chunks.
  tail -n +2 "$INPUT_FILE" | tr -d ' \r' | grep -v '^$' | sed "s/$/,$SERVICE_TIER/" \
    | split -a 3 -l "$CHUNK_SIZE" - "$WORK_DIR/chunk_"
  for part in "$WORK_DIR"/chunk_*; do
    [[ "$part" == *.csv ]] && continue
    { echo "driverId,selectedServiceTiers"; cat "$part"; } > "${part}.csv"
    rm -f "$part"
  done
  pending=( "$WORK_DIR"/chunk_*.csv )
  echo "Created ${#pending[@]} chunk files in $WORK_DIR"
fi

rm -f "$FAIL_LOG"
total=${#pending[@]}
(( total > 0 )) || { echo "Nothing to upload — all chunks already in $DONE_DIR"; exit 0; }

echo "Uploading $total chunks, $PARALLEL_JOBS in parallel..."
printf '%s\n' "${pending[@]}" | xargs -P "$PARALLEL_JOBS" -I{} bash -c 'upload_worker "$@"' _ {} || true

if [[ -f "$FAIL_LOG" ]]; then
  failed=$(wc -l < "$FAIL_LOG" | tr -d ' ')
  echo ""
  echo "DONE WITH FAILURES: $failed of $total chunks failed (details: $FAIL_LOG)."
  echo "Failed chunk files remain in $WORK_DIR — re-run the script to retry just those."
  exit 1
fi
echo "All $total chunks uploaded successfully."
