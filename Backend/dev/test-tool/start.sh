#!/bin/bash
# Start Test Dashboard + Mock Stripe + Context API
DIR="$(cd "$(dirname "$0")" && pwd)"

echo ""
echo "  ╔══════════════════════════════════════════╗"
echo "  ║   NammaYatri E2E Test Environment        ║"
echo "  ╚══════════════════════════════════════════╝"
echo ""

# Ports — chosen to avoid all nammayatri service ports (8013-8091, 6379, etc.)
DASH_PORT=7070
STRIPE_PORT=7081
CTX_PORT=7082

# Start Mock Stripe
python3 "$DIR/mock-stripe/server.py" --port $STRIPE_PORT &
PIDS=$!

# Start Context API (DB queries for test data)
python3 "$DIR/context-api/server.py" --port $CTX_PORT &
PIDS="$PIDS $!"

# Start React dashboard
if [ -d "$DIR/dashboard/build" ]; then
  npx serve -s "$DIR/dashboard/build" -l $DASH_PORT --no-clipboard &
else
  cd "$DIR/dashboard" && PORT=$DASH_PORT npm start &
fi
PIDS="$PIDS $!"

echo ""
echo "  Dashboard:     http://localhost:$DASH_PORT"
echo "  Mock Stripe:   http://localhost:$STRIPE_PORT"
echo "  Context API:   http://localhost:$CTX_PORT"
echo ""
echo "  Seed data:     cd dev/sql-seed && ./run-seed.sh"
echo "  Press Ctrl+C to stop"
echo ""

cleanup() { for p in $PIDS; do kill $p 2>/dev/null; done; wait 2>/dev/null; }
trap cleanup EXIT INT TERM
wait
