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
#   ./run-tests.sh --list                             # List available suites
#   ./run-tests.sh --check                            # Check for stuck entities

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
RIDE_DIR="$SCRIPT_DIR/collections/RideBookingFlow"
ONLINE_DIR="$SCRIPT_DIR/collections/OnlineRideBookingFlow"
BUS_DIR="$SCRIPT_DIR/collections/BusTicketBookingFlow"
METRO_DIR="$SCRIPT_DIR/collections/MetroTicketBookingFlow"
SUBWAY_DIR="$SCRIPT_DIR/collections/SubwayTicketBookingFlow"
REPORTS_DIR="$SCRIPT_DIR/reports"

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

# ── List ──

list_suites() {
    echo "=== Ride Booking Flow ==="
    for env_name in "${RIDE_ENVS[@]}"; do
        local env_file="$RIDE_DIR/Local_${env_name}.postman_environment.json"
        if [ -f "$env_file" ]; then
            echo "  $env_name:"
            for f in "$RIDE_DIR"/*.json; do
                [[ "$f" == *"postman_environment"* ]] && continue
                echo "    $(basename "$f" .json)"
            done
        fi
    done
    for label_dir in "Online Ride:$ONLINE_DIR" "Bus:$BUS_DIR" "Metro:$METRO_DIR" "Subway:$SUBWAY_DIR"; do
        local label="${label_dir%%:*}"
        local dir="${label_dir#*:}"
        echo ""
        echo "=== $label Ticket Booking Flow ==="
        [ -d "$dir" ] || continue
        for env_file in "$dir"/Local_*.postman_environment.json; do
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

    newman run "$collection" \
        -e "$env_file" \
        --bail \
        --timeout-request 15000 \
        --reporters cli \
        || { echo "FAILED: $suite_name ($env_name)"; return 1; }

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

        local env_file="$RIDE_DIR/Local_${env_name}.postman_environment.json"
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
    for env_file in "$flow_dir"/Local_*.postman_environment.json; do
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

# ── Main ──

case "${1:-}" in
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
    "")
        run_rides
        ;;
    *)
        # Backward compat: treat as suite name for NY_Bangalore
        run_rides "NY_Bangalore" "$1"
        ;;
esac
