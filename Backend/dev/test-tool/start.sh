#!/bin/bash
# Start Test Dashboard + Context API
# Mock Stripe is now served by the unified mock server at dev/mock-servers/server.py (port 8080)
DIR="$(cd "$(dirname "$0")" && pwd)"

echo ""
echo "  ╔══════════════════════════════════════════╗"
echo "  ║   NammaYatri E2E Test Environment        ║"
echo "  ╚══════════════════════════════════════════╝"
echo ""

DASH_PORT=7070
CTX_PORT=7082

# Start Context API (DB queries for test data)
python3 "$DIR/context-api/server.py" --port $CTX_PORT &
PIDS=$!

# Start React dashboard
if [ -d "$DIR/dashboard/build" ]; then
  npx serve -s "$DIR/dashboard/build" -l $DASH_PORT --no-clipboard &
else
  cd "$DIR/dashboard" && PORT=$DASH_PORT npm start &
fi
PIDS="$PIDS $!"

echo ""
echo "  Dashboard:     http://localhost:$DASH_PORT"
echo "  Context API:   http://localhost:$CTX_PORT"
echo "  Mock Stripe:   http://localhost:8080/stripe  (via unified mock server)"
echo ""
echo "  Seed data:     cd dev/sql-seed && ./run-seed.sh"
echo "  Press Ctrl+C to stop"
echo ""

cleanup() { for p in $PIDS; do kill $p 2>/dev/null; done; wait 2>/dev/null; }
trap cleanup EXIT INT TERM
wait
