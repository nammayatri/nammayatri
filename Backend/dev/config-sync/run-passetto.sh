#!/bin/bash
# Start passetto DB + service standalone (no full dev stack needed)
# Used by config_transfer.py during patch/import for ENCRYPT: re-encryption.
#
# Usage:
#   ./run-passetto.sh           # start passetto
#   ./run-passetto.sh stop      # stop passetto + DB
#   ./run-passetto.sh status    # check if running

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# flake.nix is at nammayatri/ (three levels up from dev/config-sync), not Backend/
FLAKE_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
SOCKET_DIR="$HOME/NY/socket/passetto-db"
PG_PORT=5422
PG_DATA="$HOME/NY/data/passetto-db"
SEED_SQL="$SCRIPT_DIR/../sql-seed/passetto-seed.sql"

# ── Find passetto binary ──
find_passetto() {
    # 1. Check if already set via env
    if [ -n "${PASSETTO_BIN:-}" ] && [ -d "$PASSETTO_BIN" ]; then
        return 0
    fi

    # 2. Try nix build (top-level package, if the flake exposes it)
    local pkg=""
    local system
    system=$(nix eval --impure --expr 'builtins.currentSystem' 2>/dev/null | tr -d '"') || true
    if [ -f "$FLAKE_ROOT/flake.nix" ] && [ -n "$system" ]; then
        echo "Building passetto via nix (top-level flake package)..."
        pkg=$(cd "$FLAKE_ROOT" && nix build ".#packages.${system}.passetto-service" --no-link --print-out-paths 2>/dev/null) || true
    fi

    # 2b. nammayatri does not expose passetto at top level; build locked passetto input (see flake.lock nodes.passetto)
    if { [ -z "$pkg" ] || [ ! -d "$pkg" ]; } && [ -n "$system" ] && command -v jq >/dev/null 2>&1 && [ -f "$FLAKE_ROOT/flake.lock" ]; then
        local owner repo rev
        owner=$(jq -r '.nodes.passetto.locked.owner // empty' "$FLAKE_ROOT/flake.lock")
        repo=$(jq -r '.nodes.passetto.locked.repo // empty' "$FLAKE_ROOT/flake.lock")
        rev=$(jq -r '.nodes.passetto.locked.rev // empty' "$FLAKE_ROOT/flake.lock")
        if [ -n "$owner" ] && [ -n "$repo" ] && [ -n "$rev" ]; then
            echo "Building passetto from flake.lock (github:${owner}/${repo}@${rev:0:7})..."
            pkg=$(nix build "github:${owner}/${repo}/${rev}#packages.${system}.passetto-service" --no-link --print-out-paths 2>/dev/null) || true
        fi
    fi

    # 3. Find in nix store (already built)
    if [ -z "$pkg" ] || [ ! -d "$pkg" ]; then
        # Look for passetto with passetto-server binary (the older package)
        for candidate in $(find /nix/store -maxdepth 1 -name "*passetto-service*" -type d 2>/dev/null); do
            if [ -x "$candidate/bin/passetto-server" ]; then
                pkg="$candidate"
                break
            fi
            if [ -x "$candidate/bin/passetto-service" ]; then
                pkg="$candidate"
                break
            fi
        done
    fi

    if [ -z "$pkg" ] || [ ! -d "$pkg/bin" ]; then
        echo "ERROR: Could not find passetto binary"
        echo "  Try: nix develop .#backend (from $FLAKE_ROOT)"
        return 1
    fi

    PASSETTO_BIN="$pkg/bin"

    # Determine correct binary names
    if [ -x "$PASSETTO_BIN/passetto-server" ]; then
        PASSETTO_SERVER="$PASSETTO_BIN/passetto-server"
        PASSETTO_INIT="$PASSETTO_BIN/passetto-init"
    elif [ -x "$PASSETTO_BIN/passetto-service" ]; then
        PASSETTO_SERVER="$PASSETTO_BIN/passetto-service"
        PASSETTO_INIT="$PASSETTO_BIN/passetto-service"  # May not have separate init
    else
        echo "ERROR: No passetto binary found in $PASSETTO_BIN"
        return 1
    fi

    echo "Passetto binary: $PASSETTO_SERVER"
}

stop_passetto() {
    echo "Stopping passetto..."
    pkill -f "passetto-server\|passetto-service" 2>/dev/null || true
    if [ -f "$PG_DATA/postmaster.pid" ]; then
        pg_ctl -D "$PG_DATA" stop -m fast 2>/dev/null || true
    fi
    echo "Stopped."
}

case "${1:-}" in
    stop)
        stop_passetto
        exit 0
        ;;
    status)
        if curl -s http://localhost:8079/status >/dev/null 2>&1; then
            echo "Passetto running: $(curl -s http://localhost:8079/status)"
            exit 0
        else
            echo "Passetto not running"
            exit 1
        fi
        ;;
esac

# Check if already running
if curl -s http://localhost:8079/status >/dev/null 2>&1; then
    echo "Passetto already running on :8079"
    exit 0
fi

find_passetto

# ── Start postgres for passetto ──
mkdir -p "$SOCKET_DIR" "$PG_DATA"

if [ ! -f "$PG_DATA/PG_VERSION" ]; then
    echo "Initializing passetto DB..."
    initdb -D "$PG_DATA" --no-locale --encoding=UTF8 -U passetto
fi

if ! pg_isready -h "$SOCKET_DIR" -p "$PG_PORT" -U passetto >/dev/null 2>&1; then
    echo "Starting passetto postgres on port $PG_PORT..."
    pg_ctl -D "$PG_DATA" -o "-k $SOCKET_DIR -p $PG_PORT" -l "$PG_DATA/postgres.log" start

    for i in $(seq 1 20); do
        pg_isready -h "$SOCKET_DIR" -p "$PG_PORT" -U passetto >/dev/null 2>&1 && break
        sleep 0.5
    done
fi

# Create DB if needed
if ! psql -h "$SOCKET_DIR" -p "$PG_PORT" -U passetto -d passetto -c "SELECT 1" >/dev/null 2>&1; then
    echo "Creating passetto database..."
    createdb -h "$SOCKET_DIR" -p "$PG_PORT" -U passetto passetto 2>/dev/null || true
fi

# Seed schema if needed
HAS_SCHEMA=$(psql -h "$SOCKET_DIR" -p "$PG_PORT" -U passetto -d passetto -tAc \
    "SELECT COUNT(*) FROM information_schema.schemata WHERE schema_name = 'Passetto';" 2>/dev/null || echo "0")

if [ "$HAS_SCHEMA" = "0" ]; then
    echo "Seeding passetto schema + keys..."
    psql -h "$SOCKET_DIR" -p "$PG_PORT" -U passetto -d passetto -f "$SEED_SQL"
fi

# Init keys if needed
KEY_COUNT=$(psql -h "$SOCKET_DIR" -p "$PG_PORT" -U passetto -d passetto -tAc \
    'SELECT COUNT(*) FROM "Passetto"."Keys"' 2>/dev/null || echo "0")

if [ "$KEY_COUNT" = "0" ] && [ -x "$PASSETTO_INIT" ]; then
    echo "Initializing passetto keys..."
    PASSETTO_PG_BACKEND_CONN_STRING="host=$SOCKET_DIR port=$PG_PORT dbname=passetto user=passetto" \
        "$PASSETTO_INIT" 1 3
fi

# ── Start passetto server ──
lsof -ti :8079 2>/dev/null | xargs kill -9 2>/dev/null || true
sleep 1

echo "Starting passetto server on :8079..."
PASSETTO_PG_BACKEND_CONN_STRING="host=$SOCKET_DIR port=$PG_PORT dbname=passetto user=passetto" \
MASTER_PASSWORD=1 \
PORT=8079 \
    "$PASSETTO_SERVER" > "$PG_DATA/passetto-server.log" 2>&1 &

for i in $(seq 1 20); do
    sleep 0.5
    if curl -s http://localhost:8079/status >/dev/null 2>&1; then
        echo "Passetto ready: $(curl -s http://localhost:8079/status)"
        exit 0
    fi
done

echo "ERROR: passetto failed to start. Check $PG_DATA/passetto-server.log"
exit 1
