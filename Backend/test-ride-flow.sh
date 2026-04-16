#!/bin/bash
# Test script to simulate the complete ONDC TRV:10 ride flow
# Usage: ./test-ride-flow.sh <ride_id> <driver_token> <otp>
#
# This simulates steps 9-14 of the "Assign driver on on-confirm" flow:
#   9.  on_status (RIDE_ENROUTE_PICKUP) - sent after on_confirm
#   10. track / on_track - triggered by BAP
#   12. on_status (RIDE_ARRIVED_PICKUP) - driver arrives at pickup
#   13. on_status (RIDE_STARTED) - ride starts
#   14. on_update (RIDE_ENDED) - ride ends with final fare
#
# Prerequisites: Steps 1-8 (search → on_confirm) must be completed first.
# The booking must be in TRIP_ASSIGNED status with a ride in NEW status.

RIDE_ID=${1:-""}
TOKEN=${2:-"favorit-auto2-0000000000000000000000"}
OTP=${3:-""}
BASE_URL="http://localhost:8016"
PICKUP_LAT="12.9352"
PICKUP_LON="77.6245"
DROP_LAT="12.9063"
DROP_LON="77.5857"

if [ -z "$RIDE_ID" ]; then
  echo "Usage: $0 <ride_id> [driver_token] [otp]"
  echo ""
  echo "Find ride_id: psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user -d atlas_dev -c \"SELECT id, otp, status FROM atlas_driver_offer_bpp.ride ORDER BY created_at DESC LIMIT 1;\""
  exit 1
fi

# If OTP not provided, fetch it from DB
if [ -z "$OTP" ]; then
  OTP=$(psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user -d atlas_dev -t -A -c "SELECT otp FROM atlas_driver_offer_bpp.ride WHERE id = '$RIDE_ID';")
  echo "Fetched OTP: $OTP"
fi

echo "=== ONDC TRV:10 Ride Flow Simulation ==="
echo "Ride ID: $RIDE_ID"
echo "Driver Token: $TOKEN"
echo "OTP: $OTP"
echo ""

# Step 9: RIDE_ENROUTE_PICKUP is implicit after on_confirm (RIDE_ASSIGNED)
# Our system sends RIDE_ASSIGNED on on_confirm. The buyer app's workbench
# expects the next on_status with RIDE_ENROUTE_PICKUP.
# This happens automatically when the driver's location updates show movement.
# For testing, we skip this as our system goes from ASSIGNED -> ARRIVED_PICKUP.

echo "Step 12: Driver arrives at pickup (RIDE_ARRIVED_PICKUP)"
echo "  POST $BASE_URL/ui/driver/ride/$RIDE_ID/arrived/stop"
RESULT=$(curl -s -X POST "$BASE_URL/ui/driver/ride/$RIDE_ID/arrived/stop" \
  -H "token: $TOKEN" \
  -H "Content-Type: application/json" \
  -d "{\"point\": {\"lat\": $PICKUP_LAT, \"lon\": $PICKUP_LON}}" 2>&1)
echo "  Result: $RESULT"
echo "  Waiting 5 seconds for on_status callback..."
sleep 5
echo ""

echo "Step 13: Start ride (RIDE_STARTED)"
echo "  POST $BASE_URL/ui/driver/ride/$RIDE_ID/start"
RESULT=$(curl -s -X POST "$BASE_URL/ui/driver/ride/$RIDE_ID/start" \
  -H "token: $TOKEN" \
  -H "Content-Type: application/json" \
  -d "{\"rideOtp\": \"$OTP\", \"point\": {\"lat\": $PICKUP_LAT, \"lon\": $PICKUP_LON}}" 2>&1)
echo "  Result: $RESULT"
echo "  Waiting 5 seconds for on_status callback..."
sleep 5
echo ""

echo "Step 14: End ride (RIDE_ENDED via on_update)"
echo "  POST $BASE_URL/ui/driver/ride/$RIDE_ID/end"
RESULT=$(curl -s -X POST "$BASE_URL/ui/driver/ride/$RIDE_ID/end" \
  -H "token: $TOKEN" \
  -H "Content-Type: application/json" \
  -d "{\"point\": {\"lat\": $DROP_LAT, \"lon\": $DROP_LON}}" 2>&1)
echo "  Result: $(echo $RESULT | python3 -c 'import sys,json; d=json.load(sys.stdin); print(d.get("result","ERROR: " + str(d)))' 2>/dev/null || echo $RESULT)"
echo ""

echo "=== Flow complete ==="
echo "Steps 15-16 (status/on_status) will be triggered by the buyer app."
