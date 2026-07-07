#!/usr/bin/env bash
#
# run-local-dashboard.sh — Nix-free launcher for the local test dashboard.
#
# Brings up the same two browser-facing services as `, run-local-test-dashboard`
# but using the python3 / node already on your PATH (no Nix devshell required):
#
#   test-local-api  → http://localhost:7083   (dev/test-tool/local-api/server.py)
#   test-dashboard  → http://localhost:7070   (dev/test-tool/dashboard, React)
#
# The dashboard only shows live data when a backend stack is reachable; this
# script just runs the UI + local API. Ctrl+C stops both.
#
set -euo pipefail

# ── Resolve paths (cwd-independent) ──────────────────────────────────────────
BACKEND_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"     # …/Backend
LOCAL_API="$BACKEND_DIR/dev/test-tool/local-api/server.py"
DASHBOARD_DIR="$BACKEND_DIR/dev/test-tool/dashboard"

API_PORT=7083
UI_PORT=7070

# ── Pre-flight: tools ────────────────────────────────────────────────────────
command -v python3 >/dev/null || { echo "✗ python3 not found on PATH"; exit 1; }
command -v npm     >/dev/null || { echo "✗ npm not found on PATH"; exit 1; }

# test-local-api lazily imports pyyaml (spec_loader.py) — only needed for the
# specs/Tools panel, not for boot. Try to install it, but never abort if we
# can't (e.g. PEP-668 "externally-managed" Homebrew python).
if ! python3 -c 'import yaml' 2>/dev/null; then
  echo "• pyyaml not installed (only needed for the Tools/specs panel). Trying to install…"
  if ! python3 -m pip install --user pyyaml 2>/dev/null \
     && ! python3 -m pip install --user --break-system-packages pyyaml 2>/dev/null; then
    echo "  ⚠ Could not auto-install pyyaml. The dashboard will still run; to enable"
    echo "    the specs panel later, install it in a venv or: pip install --break-system-packages pyyaml"
  fi
fi

# ── Pre-flight: free ports ───────────────────────────────────────────────────
free_port() {
  local port="$1"
  local pids
  pids="$(lsof -ti "tcp:$port" 2>/dev/null || true)"
  if [ -n "$pids" ]; then
    echo "• freeing port $port (killing: $pids)"
    echo "$pids" | xargs kill -9 2>/dev/null || true
  fi
}
echo "── Pre-flight: freeing dashboard ports ──"
free_port "$API_PORT"
free_port "$UI_PORT"

# ── Cleanup on exit ──────────────────────────────────────────────────────────
API_PID=""
cleanup() {
  echo
  echo "── Shutting down ──"
  [ -n "$API_PID" ] && kill "$API_PID" 2>/dev/null || true
  free_port "$API_PORT"
  free_port "$UI_PORT"
}
trap cleanup EXIT INT TERM

# ── Start test-local-api (background) ────────────────────────────────────────
echo "── Starting test-local-api on :$API_PORT ──"
( cd "$BACKEND_DIR" && exec python3 "$LOCAL_API" ) &
API_PID=$!

# ── Start test-dashboard (foreground) ────────────────────────────────────────
echo "── Starting test-dashboard on :$UI_PORT ──"
cd "$DASHBOARD_DIR"
[ -d node_modules ] || npm install
NODE_ENV=development PORT="$UI_PORT" BROWSER=none npm start
