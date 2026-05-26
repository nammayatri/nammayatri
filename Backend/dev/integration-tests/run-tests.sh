#!/bin/bash
# Run NammaYatri integration tests against local dev environment
#
# Usage:
#   ./run-tests.sh                                    # Run all ride booking suites for all cities
#   ./run-tests.sh rides                              # Run all ride booking suites for all cities
#   ./run-tests.sh rides NY_Bangalore                 # Run ride suites for Bangalore only
#   ./run-tests.sh rides NY_Bangalore 01-NYAutoRideFlow  # Run specific suite
#   ./run-tests.sh bus                               # Run all bus ticket booking suites for all cities
#   ./run-tests.sh bus FRFS_Chennai                   # Run bus suites for Chennai only
#   ./run-tests.sh metro                              # Run all metro ticket booking suites
#   ./run-tests.sh metro FRFS_Bangalore               # Run metro suites for Bangalore only
#   ./run-tests.sh subway                             # Run all subway ticket booking suites
#   ./run-tests.sh push-notification                  # Run push notification CRUD suites (BPP + BAP)
#   ./run-tests.sh ophub                              # Run all operation hub suites
#   ./run-tests.sh ophub NY_Bangalore                 # Run operation hub suites for Bangalore
#   ./run-tests.sh --list                             # List available suites
#   ./run-tests.sh --check                            # Check for stuck entities

set -euo pipefail

# Parse flags (can appear anywhere in args)
VERBOSE=false
VERBOSE_PRETTY=false
DEBUG_MODE=false
DEBUG_ERRORS_ONLY=false
args=()
for arg in "$@"; do
    case "$arg" in
        --verbose|-v) VERBOSE=true ;;
        --verbose-pretty|-vp) VERBOSE=true; VERBOSE_PRETTY=true ;;
        -de) DEBUG_MODE=true; DEBUG_ERRORS_ONLY=true ;;
        -d) DEBUG_MODE=true ;;
        *) args+=("$arg") ;;
    esac
done
set -- "${args[@]+"${args[@]}"}"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
RIDE_DIR="$SCRIPT_DIR/collections/RideBookingFlow"
ONLINE_DIR="$SCRIPT_DIR/collections/OnlineRideBookingFlow"
ONLINE_OFFERS_DIR="$SCRIPT_DIR/collections/OnlineRideBookingOffers"
OFFLINE_OFFERS_DIR="$SCRIPT_DIR/collections/OfflineRideBookingOffers"
BUS_DIR="$SCRIPT_DIR/collections/BusTicketBookingFlow"
METRO_DIR="$SCRIPT_DIR/collections/MetroTicketBookingFlow"
SUBWAY_DIR="$SCRIPT_DIR/collections/SubwayTicketBookingFlow"
SCHEDULER_DIR="$SCRIPT_DIR/collections/SchedulerFlow"
LOYALTY_DIR="$SCRIPT_DIR/collections/LoyaltyWalletFlow"
STCL_DIR="$SCRIPT_DIR/collections/StclMembershipFlow"
INTERCITY_DIR="$SCRIPT_DIR/collections/IntercityRideFlow"
RENTAL_DIR="$SCRIPT_DIR/collections/RentalRideFlow"
PUSH_NOTIFICATION_DIR="$SCRIPT_DIR/collections/PushNotificationCRUD"
FLEET_DIR="$SCRIPT_DIR/collections/FleetManagementFlow"
SMS_DIR="$SCRIPT_DIR/collections/KaleyraSmsFlow"
OPHUB_DIR="$SCRIPT_DIR/collections/OperationHubFlow"
AIRPORT_DIR="$SCRIPT_DIR/collections/AirportTaxiFlow"
TOLL_CONFIG_DIR="$SCRIPT_DIR/collections/TollConfigFlow"
TOLL_RIDE_DIR="$SCRIPT_DIR/collections/TollRideFlow"
REPORTS_DIR="$SCRIPT_DIR/reports"
TEST_LOGS_DIR="$SCRIPT_DIR/data/test-logs"
DEBUG_RUNNER="$SCRIPT_DIR/debug-runner.py"

DB_HOST="localhost"
DB_PORT="5434"
DB_NAME="atlas_dev"
DB_USER_SUPER="atlas_superuser"

# City environments for ride booking
RIDE_ENVS=("NY_Bangalore" "YS_Kolkata" "NY_Chennai" "BT_Delhi")

# Cities that support Cab (TAXI/SEDAN) fare products
# Bangalore only has Auto, no cab dynamic-offer fare policies
CAB_CITIES=("YS_Kolkata" "NY_Chennai" "BT_Delhi")

# ── Utilities ──

flush_redis() {
    redis-cli -p 6379 FLUSHALL > /dev/null 2>&1 || true
    for port in 30001 30002 30003 30004 30005 30006; do
        redis-cli -p $port FLUSHALL > /dev/null 2>&1 || true
    done
    # Recreate Redis stream consumer groups destroyed by FLUSHALL
    # Keys include the hedis prefix used by each app's withNonCriticalCrossAppRedis
    redis-cli -p 30001 -c XGROUP CREATE Available_Jobs myGroup 0 MKSTREAM > /dev/null 2>&1 || true
    redis-cli -p 30001 -c XGROUP CREATE Available_Jobs_Rider myGroup_Rider 0 MKSTREAM > /dev/null 2>&1 || true
    redis-cli -p 30001 -c XGROUP CREATE Available_Chakras myGroup_Chakras 0 MKSTREAM > /dev/null 2>&1 || true
    redis-cli -p 30001 -c XGROUP CREATE "ab:n_c:Available_Jobs_Rider" myGroup_Rider 0 MKSTREAM > /dev/null 2>&1 || true
}

check_stuck_entities() {
    flush_redis
    echo "Checking for stuck entities..."
    local stuck
    stuck=$(psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER_SUPER" -d "$DB_NAME" -tAc \
        "SELECT COUNT(*) FROM atlas_driver_offer_bpp.booking WHERE status NOT IN ('COMPLETED', 'CANCELLED');" 2>/dev/null || echo "0")
    if [ "$stuck" != "0" ] && [ -n "$stuck" ]; then
        echo "  WARNING: $stuck stuck bookings"
    else
        echo "  OK — no stuck entities"
    fi
}

enable_all_drivers() {
    psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER_SUPER" -d "$DB_NAME" -tAc \
        "UPDATE atlas_driver_offer_bpp.driver_information SET enabled = true, verified = true WHERE enabled = false OR verified = false;" 2>/dev/null || true
}

TOLL_SETUP_SQL="$SCRIPT_DIR/../local-testing-data/toll-dashboard-access.sql"
PROVIDER_DASHBOARD_SEED_SQL="$SCRIPT_DIR/../local-testing-data/provider-dashboard.sql"

# Toll dashboard: access_matrix + optional local-testing-data (person, token, merchant_access).
seed_toll_dashboard_access() {
    if [ "${NY_TEST_SKIP_TOLL_SEED:-}" = "1" ]; then
        echo "Skipping toll dashboard seed (NY_TEST_SKIP_TOLL_SEED=1)"
        return 0
    fi
    if [ ! -f "$TOLL_SETUP_SQL" ]; then
        echo "WARNING: Toll setup SQL not found: $TOLL_SETUP_SQL"
        return 0
    fi
    echo "Seeding toll dashboard access (access_matrix)..."
    if ! psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER_SUPER" -d "$DB_NAME" \
        -v ON_ERROR_STOP=1 -f "$TOLL_SETUP_SQL" > /dev/null 2>&1; then
        echo "WARNING: Toll access_matrix seed failed (postgres on $DB_HOST:$DB_PORT?)"
        echo "  Manual: psql -h $DB_HOST -p $DB_PORT -U $DB_USER_SUPER -d $DB_NAME -f $TOLL_SETUP_SQL"
        return 0
    fi
    if [ -f "$PROVIDER_DASHBOARD_SEED_SQL" ]; then
        echo "Seeding provider-dashboard local-testing-data (token, merchant_access)..."
        psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER_SUPER" -d "$DB_NAME" \
            -f "$PROVIDER_DASHBOARD_SEED_SQL" > /dev/null 2>&1 || \
            echo "WARNING: provider-dashboard.sql seed failed — run manually if toll tests get 403"
    fi
}

setup() {
    echo "=== Integration test setup ==="
    seed_toll_dashboard_access
    echo "Done. Run: ./run-tests.sh toll-config NY_Bangalore"
}

# ── List ──

list_suites() {
    echo "=== Ride Booking Flow ==="
    for env_name in "${RIDE_ENVS[@]}"; do
        local env_file="$RIDE_DIR/Local/Local_${env_name}.postman_environment.json"
        if [ -f "$env_file" ]; then
            echo "  $env_name:"
            for f in "$RIDE_DIR"/*.json; do
                [[ "$f" == *"postman_environment"* ]] && continue
                echo "    $(basename "$f" .json)"
            done
        fi
    done
    for label_dir in "Toll Config:$TOLL_CONFIG_DIR" "Toll Ride:$TOLL_RIDE_DIR" "Online Ride:$ONLINE_DIR" "Bus:$BUS_DIR" "Metro:$METRO_DIR" "Subway:$SUBWAY_DIR" "Scheduler:$SCHEDULER_DIR" "Fleet Management:$FLEET_DIR"; do
        local label="${label_dir%%:*}"
        local dir="${label_dir#*:}"
        echo ""
        echo "=== $label Ticket Booking Flow ==="
        [ -d "$dir" ] || continue
        for env_file in "$dir"/Local/Local_*.postman_environment.json; do
            [ -f "$env_file" ] || continue
            local env_name
            env_name=$(basename "$env_file" .postman_environment.json | sed 's/^Local_//')
            echo "  $env_name:"
            for f in "$dir"/*.json; do
                [[ "$f" == *"postman_environment"* ]] && continue
                echo "    $(basename "$f" .json)"
            done
        done
    done
}

# ── Pretty print JSON report (request/response) ──

_pretty_print_json_report() {
    local json_file="$1"
    [ -f "$json_file" ] || return 0
    python3 -c "
import json, sys

with open('$json_file') as f:
    report = json.load(f)

for item in report.get('run', {}).get('executions', []):
    name = item.get('item', {}).get('name', '?')
    req = item.get('request', {})
    resp = item.get('response', {})

    method = req.get('method', '?')
    url = req.get('url', {})
    url_str = url if isinstance(url, str) else url.get('raw', str(url))

    print(f'\n\033[1m→ {name}\033[0m')
    print(f'  {method} {url_str}')

    # Request body
    body = req.get('body', {})
    if body and body.get('raw'):
        print(f'  \033[36m↑ Request:\033[0m')
        try:
            print(json.dumps(json.loads(body['raw']), indent=2, ensure_ascii=False))
        except (json.JSONDecodeError, TypeError):
            print(f'  {body[\"raw\"]}')

    # Response
    status = resp.get('code', '?')
    resp_body = resp.get('body', '') or resp.get('stream', {})
    print(f'  \033[33m↓ Response ({status}):\033[0m')
    if isinstance(resp_body, str) and resp_body:
        try:
            print(json.dumps(json.loads(resp_body), indent=2, ensure_ascii=False))
        except (json.JSONDecodeError, TypeError):
            print(f'  {resp_body[:500]}')
    elif isinstance(resp_body, dict) and resp_body.get('data'):
        try:
            text = bytes(resp_body['data']).decode('utf-8')
            print(json.dumps(json.loads(text), indent=2, ensure_ascii=False))
        except Exception:
            print(f'  (binary data, {len(resp_body.get(\"data\", []))} bytes)')
" 2>/dev/null || true
}

# ── Run a single collection with a given environment ──

run_single() {
    local collection="$1"
    local env_file="$2"
    local suite_name
    suite_name=$(basename "$collection" .json)
    local env_name
    env_name=$(basename "$env_file" .json)

    flush_redis
    enable_all_drivers

    mkdir -p "$REPORTS_DIR"
    echo "Running: $suite_name ($env_name)"

    # Debug mode: delegate to Python debug runner for per-API log capture
    if [ "$DEBUG_MODE" = true ]; then
        local debug_flags=()
        if [ "$DEBUG_ERRORS_ONLY" = true ]; then
            debug_flags+=(--errors-only)
        fi
        if [ "$VERBOSE" = true ]; then
            debug_flags+=(--verbose)
        fi
        python3 "$DEBUG_RUNNER" "$collection" "$env_file" "$TEST_LOGS_DIR" \
            --bail --timeout-request 60000 \
            "${debug_flags[@]+"${debug_flags[@]}"}" \
            || { echo "FAILED: $suite_name ($env_name)"; return 1; }
        echo "PASSED: $suite_name ($env_name)"
        return 0
    fi

    local verbose_flags=()
    if [ "$VERBOSE" = true ]; then
        verbose_flags+=(--verbose)
    fi

    if [ "$VERBOSE_PRETTY" = true ]; then
        # Run with JSON reporter, then pretty-print request/response
        local json_out="$REPORTS_DIR/${suite_name}_${env_name}.json"
        newman run "$collection" \
            -e "$env_file" \
            --bail \
            --timeout-request 60000 \
            --reporters cli,json \
            --reporter-json-export "$json_out" \
            "${verbose_flags[@]+"${verbose_flags[@]}"}" \
            || { _pretty_print_json_report "$json_out"; echo "FAILED: $suite_name ($env_name)"; return 1; }
        _pretty_print_json_report "$json_out"
    else
        newman run "$collection" \
            -e "$env_file" \
            --bail \
            --timeout-request 60000 \
            --reporters cli \
            "${verbose_flags[@]+"${verbose_flags[@]}"}" \
            || { echo "FAILED: $suite_name ($env_name)"; return 1; }
    fi

    echo "PASSED: $suite_name ($env_name)"
}

# ── Ride Booking ──

run_rides() {
    local filter_env="${1:-}"
    local filter_suite="${2:-}"

    if [ ! -d "$RIDE_DIR" ]; then
        echo "No ride collections found at $RIDE_DIR"
        exit 1
    fi

    check_stuck_entities

    local passed=0
    local failed=0
    local failed_suites=""

    for env_name in "${RIDE_ENVS[@]}"; do
        # Filter by env if specified
        if [ -n "$filter_env" ] && [ "$filter_env" != "$env_name" ]; then
            continue
        fi

        local env_file="$RIDE_DIR/Local/Local_${env_name}.postman_environment.json"
        if [ ! -f "$env_file" ]; then
            echo "WARNING: Environment not found: $env_file, skipping $env_name"
            continue
        fi

        echo ""
        echo "════════════════════════════════════════════════════════════"
        echo "  $env_name"
        echo "════════════════════════════════════════════════════════════"

        for f in "$RIDE_DIR"/*.json; do
            [[ "$f" == *"postman_environment"* ]] && continue
            local suite_name
            suite_name=$(basename "$f" .json)

            # Filter by suite if specified
            if [ -n "$filter_suite" ] && [ "$filter_suite" != "$suite_name" ]; then
                continue
            fi

            # Skip Cab collections for cities without cab fare policies
            if [[ "$suite_name" == *"Cab"* ]]; then
                local is_cab_city=false
                for cc in "${CAB_CITIES[@]}"; do
                    [ "$cc" == "$env_name" ] && is_cab_city=true
                done
                if [ "$is_cab_city" = false ]; then
                    echo "  SKIP: $suite_name (no cab fare policies for $env_name)"
                    continue
                fi
            fi

            echo ""
            echo "------------------------------------------------------------"
            echo "  $env_name / $suite_name"
            echo "------------------------------------------------------------"

            if run_single "$f" "$env_file"; then
                passed=$((passed + 1))
            else
                failed=$((failed + 1))
                failed_suites="$failed_suites $env_name/$suite_name"
            fi
        done
    done

    echo ""
    echo "════════════════════════════════════════════════════════════"
    echo "  RIDE RESULTS: $passed passed, $failed failed"
    if [ -n "$failed_suites" ]; then
        echo "  Failed:$failed_suites"
    fi
    echo "════════════════════════════════════════════════════════════"
    [ "$failed" -eq 0 ]
}

# ── FRFS Ticket Booking (generic for bus/metro/subway) ──

run_frfs() {
    local flow_dir="$1"
    local flow_label="$2"
    local filter_env="${3:-}"
    local filter_suite="${4:-}"

    if [ ! -d "$flow_dir" ]; then
        echo "No collections found at $flow_dir"
        exit 1
    fi

    local passed=0
    local failed=0
    local failed_suites=""

    # Iterate over environment files
    for env_file in "$flow_dir"/Local/Local_*.postman_environment.json; do
        [ -f "$env_file" ] || continue
        local env_name
        env_name=$(basename "$env_file" .postman_environment.json | sed 's/^Local_//')

        if [ -n "$filter_env" ] && [ "$filter_env" != "$env_name" ]; then
            continue
        fi

        echo ""
        echo "════════════════════════════════════════════════════════════"
        echo "  $flow_label / $env_name"
        echo "════════════════════════════════════════════════════════════"

        for f in "$flow_dir"/*.json; do
            [[ "$f" == *"postman_environment"* ]] && continue
            local suite_name
            suite_name=$(basename "$f" .json)

            if [ -n "$filter_suite" ] && [ "$filter_suite" != "$suite_name" ]; then
                continue
            fi

            echo ""
            echo "------------------------------------------------------------"
            echo "  $env_name / $suite_name"
            echo "------------------------------------------------------------"

            if run_single "$f" "$env_file"; then
                passed=$((passed + 1))
            else
                failed=$((failed + 1))
                failed_suites="$failed_suites $env_name/$suite_name"
            fi
        done
    done

    echo ""
    echo "════════════════════════════════════════════════════════════"
    echo "  $flow_label RESULTS: $passed passed, $failed failed"
    if [ -n "$failed_suites" ]; then
        echo "  Failed:$failed_suites"
    fi
    echo "════════════════════════════════════════════════════════════"
    [ "$failed" -eq 0 ]
}

run_bus() { run_frfs "$BUS_DIR" "BUS" "${1:-}" "${2:-}"; }
run_metro() { run_frfs "$METRO_DIR" "METRO" "${1:-}" "${2:-}"; }
run_subway() { run_frfs "$SUBWAY_DIR" "SUBWAY" "${1:-}" "${2:-}"; }
run_online() { run_frfs "$ONLINE_DIR" "ONLINE RIDE" "${1:-}" "${2:-}"; }
run_online_offers() { run_frfs "$ONLINE_OFFERS_DIR" "ONLINE RIDE OFFERS" "${1:-}" "${2:-}"; }
run_offline_offers() { run_frfs "$OFFLINE_OFFERS_DIR" "OFFLINE RIDE OFFERS" "${1:-}" "${2:-}"; }
# Scheduler tests skip flush_redis — FLUSHALL kills Redis stream consumer groups
# and scheduler threads don't recover. Each scheduler suite is responsible for
# its own targeted cleanup via POST /mock/scheduler/clear (see SchedulerFlow/README.md).
run_scheduler() {
    local filter_env="${1:-}"
    local filter_suite="${2:-}"
    local passed=0 failed=0 failed_suites=""

    for env_file in "$SCHEDULER_DIR"/Local/Local_*.postman_environment.json; do
        [ -f "$env_file" ] || continue
        local env_name
        env_name=$(basename "$env_file" .postman_environment.json | sed 's/^Local_//')
        [ -n "$filter_env" ] && [ "$filter_env" != "$env_name" ] && continue

        echo ""
        echo "════════════════════════════════════════════════════════════"
        echo "  SCHEDULER / $env_name"
        echo "════════════════════════════════════════════════════════════"

        for f in "$SCHEDULER_DIR"/*.json; do
            [[ "$f" == *"postman_environment"* ]] && continue
            local suite_name
            suite_name=$(basename "$f" .json)
            [ -n "$filter_suite" ] && [ "$filter_suite" != "$suite_name" ] && continue

            echo ""
            echo "------------------------------------------------------------"
            echo "  $env_name / $suite_name"
            echo "------------------------------------------------------------"
            echo "Running: $suite_name ($env_name)"

            if newman run "$f" -e "$env_file" --bail --timeout-request 60000 --reporters cli; then
                echo "PASSED: $suite_name ($env_name)"
                passed=$((passed + 1))
            else
                echo "FAILED: $suite_name ($env_name)"
                failed=$((failed + 1))
                failed_suites="$failed_suites $env_name/$suite_name"
            fi
        done
    done

    echo ""
    echo "════════════════════════════════════════════════════════════"
    echo "  SCHEDULER RESULTS: $passed passed, $failed failed"
    if [ -n "$failed_suites" ]; then echo "  Failed:$failed_suites"; fi
    echo "════════════════════════════════════════════════════════════"
    [ "$failed" -eq 0 ]
}

run_loyalty() { run_frfs "$LOYALTY_DIR" "LOYALTY WALLET" "${1:-}" "${2:-}"; }
run_stcl() { run_frfs "$STCL_DIR" "STCL MEMBERSHIP" "${1:-}" "${2:-}"; }
run_intercity() { run_frfs "$INTERCITY_DIR" "INTERCITY" "${1:-}" "${2:-}"; }
run_rental() { run_frfs "$RENTAL_DIR" "RENTAL" "${1:-}" "${2:-}"; }
run_push_notification() { run_frfs "$PUSH_NOTIFICATION_DIR" "PUSH NOTIFICATION CRUD" "${1:-}" "${2:-}"; }
run_fleet() { run_frfs "$FLEET_DIR" "FLEET MANAGEMENT" "${1:-}" "${2:-}"; }
run_sms() {
    echo ""
    echo "  NOTE: OTP tests run with useFakeSms (no real SMS sent)."
    echo "  Resend OTP tests (steps 06-10) are SKIPPED by default as they"
    echo "  require real SMS provider credentials."
    echo ""
    local sms_exit=0
    run_frfs "$SMS_DIR" "KALEYRA SMS" "${1:-}" "${2:-}" || sms_exit=$?
    echo ""
    echo "  ┌──────────────────────────────────────────────────────────────────┐"
    echo "  │  To run resend OTP tests with real SMS provider credentials:    │"
    echo "  │    1. Add real Kaleyra/GupShup credentials to your config       │"
    echo "  │    2. Set enable_real_sms_tests=true in your environment file   │"
    echo "  │       (e.g. Local_NY_Bangalore.postman_environment.json)        │"
    echo "  │    3. Re-run: ./run-tests.sh sms                               │"
    echo "  └──────────────────────────────────────────────────────────────────┘"
    echo ""
    return $sms_exit
}
run_ophub() { run_frfs "$OPHUB_DIR" "OPERATION HUB" "${1:-}" "${2:-}"; }
run_airport() { run_frfs "$AIRPORT_DIR" "AIRPORT TAXI" "${1:-}" "${2:-}"; }
run_toll_config() {
    seed_toll_dashboard_access
    run_frfs "$TOLL_CONFIG_DIR" "TOLL CONFIG" "${1:-}" "${2:-}"
}
run_toll_ride() {
    seed_toll_dashboard_access
    run_frfs "$TOLL_RIDE_DIR" "TOLL RIDE" "${1:-}" "${2:-}"
}
run_toll() {
    run_toll_config "${1:-}" "${2:-}" && run_toll_ride "${1:-}" "${2:-}"
}

# ── Help ──

show_help() {
    echo "Usage: ./run-tests.sh [COMMAND] [CITY] [SUITE]"
    echo ""
    echo "Run integration tests via Newman against the local dev environment."
    echo ""
    echo "Commands:"
    echo "  (none)              Run all ride booking suites for all cities"
    echo "  rides               Run all ride booking suites for all cities"
    echo "  bus                 Run all bus ticket booking suites"
    echo "  metro               Run all metro ticket booking suites"
    echo "  subway              Run all subway ticket booking suites"
    echo "  online              Run online (Stripe) ride booking suites"
    echo "  online-offers       Run online ride discount offer suites"
    echo "  offline-offers      Run offline ride cashback offer suites"
    echo "  scheduler           Run scheduler job integration tests"
    echo "  loyalty             Run loyalty wallet topup/burn suites"
    echo "  stcl                Run STCL membership share-purchase suites (partial + full)"
    echo "  intercity           Run intercity ride suites (Bangalore -> Mysore, normal + airport OTP)"
    echo "  rental              Run rental ride suites (Bangalore 4hr/40km, normal + airport OTP)"
    echo "  push-notification   Run push notification CRUD suites (list, upsert, toggle, delete)"
    echo "  fleet               Run fleet management suites (driver name, association)"
    echo "  sms|kaleyra         Run Kaleyra SMS integration tests (non-OTP needs test_phone_number in env)"
    echo "  ophub               Run operation hub suites (hub requests, driver mobile search)"
    echo "  airport             Run airport taxi booth flow suites (Delhi BHARAT_TAXI)"
    echo "  toll-config         Run toll dashboard API suites (CRUD, CSV, polygon gates)"
    echo "  toll-ride           Run toll + auto ride flow (estimate tollChargesInfo)"
    echo "  toll                Run toll-config then toll-ride"
    echo "  ./run-tests.sh toll-config NY_Bangalore       # Toll dashboard APIs (Bangalore)"
    echo "  ./run-tests.sh toll-config BT_Delhi           # Toll dashboard APIs (Delhi)"
    echo "  ./run-tests.sh toll-ride NY_Bangalore           # Toll on estimate + auto ride (Bangalore)"
    echo "  ./run-tests.sh toll-ride BT_Delhi             # Toll on estimate + auto ride (Delhi)"
    echo "  --setup             Seed toll dashboard access_matrix + provider-dashboard.sql"
    echo "  --list              List all available suites and cities"
    echo "  --check             Check for stuck DB entities"
    echo "  -d                  Debug: capture per-API service logs to assets/test-logs/"
    echo "  -de                 Debug errors: capture service logs only for failed APIs"
    echo "  --verbose, -v       Show API request/response details"
    echo "  --verbose-pretty, -vp  Full request/response JSON pretty-printed"
    echo "  --help, -h          Show this help"
    echo ""
    echo "Examples (top-level to granular):"
    echo "  ./run-tests.sh                                    # All ride suites, all cities"
    echo "  ./run-tests.sh rides                              # All ride suites, all cities"
    echo "  ./run-tests.sh rides NY_Bangalore                 # All ride suites for Bangalore"
    echo "  ./run-tests.sh rides NY_Bangalore 01-AutoRideFlow # Specific suite + city"
    echo "  ./run-tests.sh bus                                # All bus suites, all cities"
    echo "  ./run-tests.sh bus FRFS_Chennai                   # Bus suites for Chennai"
    echo "  ./run-tests.sh metro FRFS_Bangalore               # Metro suites for Bangalore"
    echo "  ./run-tests.sh subway                             # All subway suites"
    echo "  ./run-tests.sh online                             # Online (Stripe) ride suites"
    echo "  ./run-tests.sh online-offers                      # Online discount offer suites"
    echo "  ./run-tests.sh offline-offers                     # Offline cashback offer suites"
    echo "  ./run-tests.sh loyalty                            # All loyalty wallet suites"
    echo "  ./run-tests.sh loyalty FRFS_Chennai               # Loyalty suites for Chennai"
    echo "  ./run-tests.sh loyalty FRFS_Chennai 01-WalletRechargeTopup  # Specific suite"
    echo "  ./run-tests.sh stcl                               # All STCL membership suites"
    echo "  ./run-tests.sh stcl NY_Bangalore                  # STCL suites for Bangalore"
    echo "  ./run-tests.sh stcl NY_Bangalore 01-StclMembershipPartialPurchaseFlow  # Specific suite"
    echo "  ./run-tests.sh intercity                          # All intercity suites, all cities"
    echo "  ./run-tests.sh intercity NY_Bangalore             # Intercity suites for Bangalore"
    echo "  ./run-tests.sh intercity NY_Bangalore 01-IntercityRideFlow  # Specific intercity suite"
    echo "  ./run-tests.sh rental                             # All rental suites, all cities"
    echo "  ./run-tests.sh rental NY_Bangalore 01-RentalRideFlow        # Specific rental suite"
    echo "  ./run-tests.sh push-notification                           # All push notification CRUD suites"
    echo "  ./run-tests.sh push-notification NY_Bangalore              # PN CRUD for Bangalore"
    echo "  ./run-tests.sh fleet                              # All fleet management suites"
    echo "  ./run-tests.sh fleet NY_Bangalore                 # Fleet suites for Bangalore"
    echo "  ./run-tests.sh sms                                          # Kaleyra SMS (OTP only by default)"
    echo "  ./run-tests.sh sms NY_Bangalore                             # Kaleyra SMS for Bangalore"
    echo "  ./run-tests.sh airport                            # All airport taxi suites"
    echo "  ./run-tests.sh airport BT_Delhi                   # Airport taxi suites for Delhi"
    echo "  ./run-tests.sh rides NY_Bangalore -v              # Verbose — show request/response"
    echo "  ./run-tests.sh online BF_Helsinki -vp             # Pretty-print full JSON request/response"
    echo "  ./run-tests.sh online BF_Helsinki -d              # Debug: per-API service logs for all APIs"
    echo "  ./run-tests.sh online BF_Helsinki -de             # Debug: per-API service logs for errors only"
}

# ── Main ──

case "${1:-}" in
    --help|-h)
        show_help
        ;;
    --setup)
        setup
        ;;
    --list)
        list_suites
        ;;
    --check)
        check_stuck_entities
        ;;
    rides)
        run_rides "${2:-}" "${3:-}"
        ;;
    bus)
        run_bus "${2:-}" "${3:-}"
        ;;
    metro)
        run_metro "${2:-}" "${3:-}"
        ;;
    subway)
        run_subway "${2:-}" "${3:-}"
        ;;
    online)
        run_online "${2:-}" "${3:-}"
        ;;
    online-offers)
        run_online_offers "${2:-}" "${3:-}"
        ;;
    offline-offers)
        run_offline_offers "${2:-}" "${3:-}"
        ;;
    scheduler)
        run_scheduler "${2:-}" "${3:-}"
        ;;
    loyalty|wallet)
        run_loyalty "${2:-}" "${3:-}"
        ;;
    stcl|stcl-membership)
        run_stcl "${2:-}" "${3:-}"
        ;;
    intercity)
        run_intercity "${2:-}" "${3:-}"
        ;;
    rental)
        run_rental "${2:-}" "${3:-}"
        ;;
    push-notification|pn)
        run_push_notification "${2:-}" "${3:-}"
        ;;
    fleet)
        run_fleet "${2:-}" "${3:-}"
        ;;
    sms|kaleyra)
        run_sms "${2:-}" "${3:-}"
        ;;
    ophub)
        run_ophub "${2:-}" "${3:-}"
        ;;
    airport|airport-taxi)
        run_airport "${2:-}" "${3:-}"
        ;;
    toll-config)
        run_toll_config "${2:-}" "${3:-}"
        ;;
    toll-ride)
        run_toll_ride "${2:-}" "${3:-}"
        ;;
    toll)
        run_toll "${2:-}" "${3:-}"
        ;;
    "")
        run_rides
        ;;
    *)
        # Backward compat: treat as suite name for NY_Bangalore
        run_rides "NY_Bangalore" "$1"
        ;;
esac
