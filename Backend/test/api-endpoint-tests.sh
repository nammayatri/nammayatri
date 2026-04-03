#!/bin/bash
# Individual API Endpoint Tests for Chat to Ride Booking Flow
# Tests each API endpoint individually with detailed output

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
RESULTS_FILE="$SCRIPT_DIR/results/api-test-results-$(date +%Y%m%d-%H%M%S).json"
mkdir -p "$SCRIPT_DIR/results"

# Configuration
DRIVER_BASE_URL="http://localhost:8016/ui"
RIDER_BASE_URL="http://localhost:8013/v2"
LTS_BASE_URL="http://localhost:8081/ui"
DASHBOARD_BASE_URL="http://localhost:8018"
DASHBOARD_INTERNAL_URL="http://localhost:8018/bpp/driver-offer"

# Test data
TEST_ID=$(date +%s)
DRIVER_NUMBER="9$(shuf -i 100000000-999999999 -n 1)"
RIDER_NUMBER="8$(shuf -i 100000000-999999999 -n 1)"
REG_NO="KA$(shuf -i 10-99 -n 1)$(cat /dev/urandom | tr -dc 'A-Z' | head -c 2)$(shuf -i 1000-9999 -n 1)"

echo "=========================================="
echo "API Endpoint Tests"
echo "=========================================="
echo "Test ID: $TEST_ID"
echo "Results: $RESULTS_FILE"
echo "=========================================="

# Initialize results
RESULTS='{"testRunId": "'$TEST_ID'", "startTime": "'$(date -Iseconds)'", "tests": []}'

# Helper function to test an endpoint
test_endpoint() {
  local name="$1"
  local method="$2"
  local url="$3"
  local headers="${4:-}"
  local body="${5:-}"
  local expected_status="${6:-200}"
  
  echo ""
  echo "Testing: $name"
  echo "  $method $url"
  
  local start_time=$(date +%s%N)
  local http_code
  local response_body
  local curl_error
  
  # Build curl command
  local curl_cmd="curl -s -S --max-time 30 -w '\n%{http_code}'"
  
  if [[ -n "$headers" ]]; then
    # Split headers and add each with -H
    local header_array=($headers)
    for header in "${header_array[@]}"; do
      curl_cmd="$curl_cmd -H '$header'"
    done
  fi
  
  if [[ -n "$body" ]]; then
    curl_cmd="$curl_cmd -d '$body'"
  fi
  
  curl_cmd="$curl_cmd -X $method '$url'"
  
  # Execute request
  local full_response
  full_response=$(eval "$curl_cmd" 2>&1) || {
    curl_error=$?
    echo "  ✗ FAILED: Curl error (exit code: $curl_error)"
    RESULTS=$(echo "$RESULTS" | jq --arg name "$name" \
      --arg method "$method" \
      --arg url "$url" \
      --arg error "Curl exit code: $curl_error" \
      '.tests += [{"name": $name, "method": $method, "url": $url, "status": "FAILED", "error": $error}]')
    return 1
  }
  
  local end_time=$(date +%s%N)
  local duration=$(( (end_time - start_time) / 1000000 )) # Convert to milliseconds
  
  http_code=$(echo "$full_response" | tail -n1)
  response_body=$(echo "$full_response" | sed '$d')
  
  # Truncate response body for logging
  local truncated_body="${response_body:0:500}"
  if [[ ${#response_body} -gt 500 ]]; then
    truncated_body="${truncated_body}... (truncated)"
  fi
  
  if [[ "$http_code" == "$expected_status" ]]; then
    echo "  ✓ PASSED (HTTP $http_code, ${duration}ms)"
    echo "  Response: $truncated_body"
    RESULTS=$(echo "$RESULTS" | jq --arg name "$name" \
      --arg method "$method" \
      --arg url "$url" \
      --argjson status_code "$http_code" \
      --argjson duration "$duration" \
      --arg body "$truncated_body" \
      '.tests += [{"name": $name, "method": $method, "url": $url, "status": "PASSED", "httpCode": $status_code, "durationMs": $duration, "responsePreview": $body}]')
    return 0
  else
    echo "  ✗ FAILED (Expected $expected_status, got $http_code, ${duration}ms)"
    echo "  Response: $truncated_body"
    RESULTS=$(echo "$RESULTS" | jq --arg name "$name" \
      --arg method "$method" \
      --arg url "$url" \
      --argjson status_code "$http_code" \
      --argjson expected "$expected_status" \
      --argjson duration "$duration" \
      --arg body "$truncated_body" \
      '.tests += [{"name": $name, "method": $method, "url": $url, "status": "FAILED", "httpCode": $status_code, "expectedCode": $expected, "durationMs": $duration, "responsePreview": $body}]')
    return 1
  fi
}

# ============================================
# DRIVER AUTHENTICATION APIs
# ============================================
echo ""
echo "=========================================="
echo "DRIVER AUTHENTICATION APIs"
echo "=========================================="

# Driver Auth
test_endpoint "Driver Auth" "POST" "$DRIVER_BASE_URL/auth" \
  "Content-Type: application/json" \
  "{\"mobileNumber\": \"$DRIVER_NUMBER\", \"mobileCountryCode\": \"+91\", \"merchantId\": \"7f7896dd-787e-4a0b-8675-e9e6fe93bb8f\", \"merchantOperatingCity\": \"Bangalore\"}"

DRIVER_AUTH_ID=$(echo "$RESULTS" | jq -r '.tests[-1].responsePreview | fromjson? | .authId // empty' 2>/dev/null || echo "")

# Driver OTP Verify
test_endpoint "Driver OTP Verify" "POST" "$DRIVER_BASE_URL/auth/${DRIVER_AUTH_ID:-test}/verify" \
  "Content-Type: application/json" \
  "{\"otp\": \"7891\", \"deviceToken\": \"test-device-$TEST_ID\"}"

DRIVER_TOKEN=$(echo "$RESULTS" | jq -r '.tests[-1].responsePreview | fromjson? | .token // empty' 2>/dev/null || echo "")
DRIVER_ID=$(echo "$RESULTS" | jq -r '.tests[-1].responsePreview | fromjson? | .person.id // empty' 2>/dev/null || echo "")

# ============================================
# RIDER AUTHENTICATION APIs
# ============================================
echo ""
echo "=========================================="
echo "RIDER AUTHENTICATION APIs"
echo "=========================================="

# Rider Auth
test_endpoint "Rider Auth" "POST" "$RIDER_BASE_URL/auth" \
  "Content-Type: application/json" \
  "{\"mobileNumber\": \"$RIDER_NUMBER\", \"mobileCountryCode\": \"+91\", \"merchantId\": \"NAMMA_YATRI\"}"

RIDER_AUTH_ID=$(echo "$RESULTS" | jq -r '.tests[-1].responsePreview | fromjson? | .authId // empty' 2>/dev/null || echo "")

# Rider OTP Verify
test_endpoint "Rider OTP Verify" "POST" "$RIDER_BASE_URL/auth/${RIDER_AUTH_ID:-test}/verify" \
  "Content-Type: application/json" \
  "{\"otp\": \"7891\", \"deviceToken\": \"test-rider-$TEST_ID\"}"

RIDER_TOKEN=$(echo "$RESULTS" | jq -r '.tests[-1].responsePreview | fromjson? | .token // empty' 2>/dev/null || echo "")

# ============================================
# DRIVER PROFILE APIs
# ============================================
echo ""
echo "=========================================="
echo "DRIVER PROFILE APIs"
echo "=========================================="

if [[ -n "$DRIVER_TOKEN" ]]; then
  # Get Driver Profile
  test_endpoint "Get Driver Profile" "GET" "$DRIVER_BASE_URL/driver/profile" \
    "token: $DRIVER_TOKEN"
  
  # Get Driver Stats
  test_endpoint "Get Driver Stats" "GET" "$DRIVER_BASE_URL/driver/stats" \
    "token: $DRIVER_TOKEN"
  
  # Get Driver Messages
  test_endpoint "Get Driver Messages" "GET" "$DRIVER_BASE_URL/message/list?limit=5" \
    "token: $DRIVER_TOKEN"
else
  echo "Skipping Driver Profile APIs - no token available"
fi

# ============================================
# RIDER PROFILE APIs
# ============================================
echo ""
echo "=========================================="
echo "RIDER PROFILE APIs"
echo "=========================================="

if [[ -n "$RIDER_TOKEN" ]]; then
  # Get Rider Profile
  test_endpoint "Get Rider Profile" "GET" "$RIDER_BASE_URL/profile" \
    "token: $RIDER_TOKEN"
  
  # Get Rider Bookings
  test_endpoint "Get Rider Bookings" "GET" "$RIDER_BASE_URL/rideBooking/list?limit=5" \
    "token: $RIDER_TOKEN"
else
  echo "Skipping Rider Profile APIs - no token available"
fi

# ============================================
# DASHBOARD APIs
# ============================================
echo ""
echo "=========================================="
echo "DASHBOARD APIs"
echo "=========================================="

# Switch Merchant/City
test_endpoint "Dashboard Switch City" "POST" "$DASHBOARD_BASE_URL/user/switchMerchantAndCity" \
  "Content-Type: application/json token: local-admin-token-bangalore-namma-yatri" \
  "{\"merchantId\": \"NAMMA_YATRI_PARTNER\", \"city\": \"Bangalore\"}"

if [[ -n "$DRIVER_ID" ]]; then
  # Add Vehicle
  test_endpoint "Dashboard Add Vehicle" "POST" "$DASHBOARD_INTERNAL_URL/NAMMA_YATRI_PARTNER/Bangalore/driver/$DRIVER_ID/addVehicle" \
    "Content-Type: application/json token: local-admin-token-bangalore-namma-yatri" \
    "{\"registrationNo\": \"$REG_NO\", \"vehicleClass\": \"3w\", \"colour\": \"Green\", \"model\": \"AUTO RICKSHAW\", \"make\": \"Bajaj\", \"vehicleCategory\": \"AUTO_CATEGORY\"}"
  
  # Enable Driver
  test_endpoint "Dashboard Enable Driver" "POST" "$DASHBOARD_INTERNAL_URL/NAMMA_YATRI_PARTNER/Bangalore/driver/$DRIVER_ID/enable" \
    "Content-Type: application/json token: local-admin-token-bangalore-namma-yatri" \
    ""
fi

# ============================================
# LOCATION TRACKING APIs
# ============================================
echo ""
echo "=========================================="
echo "LOCATION TRACKING APIs"
echo "=========================================="

if [[ -n "$DRIVER_ID" ]]; then
  CURRENT_TIME=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
  
  # Update Driver Location
  test_endpoint "Update Driver Location" "POST" "$LTS_BASE_URL/driver/location" \
    "Content-Type: application/json token: $DRIVER_ID vt: AUTO_RICKSHAW dm: ONLINE mId: 7f7896dd-787e-4a0b-8675-e9e6fe93bb8f" \
    "[{\"pt\": {\"lat\": 12.9352, \"lon\": 77.6245}, \"ts\": \"$CURRENT_TIME\"}]"
fi

# ============================================
# DRIVER ACTIVITY APIs
# ============================================
echo ""
echo "=========================================="
echo "DRIVER ACTIVITY APIs"
echo "=========================================="

if [[ -n "$DRIVER_TOKEN" ]]; then
  # Set Driver Online
  test_endpoint "Set Driver Online" "POST" "$DRIVER_BASE_URL/driver/setActivity?active=true&mode=%22ONLINE%22" \
    "Content-Type: application/json token: $DRIVER_TOKEN" \
    ""
  
  # Get Nearby Ride Requests
  test_endpoint "Get Nearby Ride Requests" "GET" "$DRIVER_BASE_URL/driver/nearbyRideRequest" \
    "token: $DRIVER_TOKEN"
fi

# ============================================
# RIDE SEARCH APIs
# ============================================
echo ""
echo "=========================================="
echo "RIDE SEARCH APIs"
echo "=========================================="

if [[ -n "$RIDER_TOKEN" ]]; then
  # Create Ride Search
  test_endpoint "Create Ride Search" "POST" "$RIDER_BASE_URL/rideSearch" \
    "Content-Type: application/json token: $RIDER_TOKEN" \
    "{\"fareProductType\": \"ONE_WAY\", \"contents\": {\"origin\": {\"address\": {\"area\": \"Test Origin\", \"areaCode\": \"000000\", \"city\": \"Bangalore\", \"country\": \"India\", \"state\": \"Karnataka\"}, \"gps\": {\"lat\": 12.9352, \"lon\": 77.6245}}, \"destination\": {\"address\": {\"area\": \"Test Destination\", \"areaCode\": \"000000\", \"city\": \"Bangalore\", \"country\": \"India\", \"state\": \"Karnataka\"}, \"gps\": {\"lat\": 12.9716, \"lon\": 77.6412}}}}"
  
  SEARCH_ID=$(echo "$RESULTS" | jq -r '.tests[-1].responsePreview | fromjson? | .searchId // empty' 2>/dev/null || echo "")
  
  if [[ -n "$SEARCH_ID" ]]; then
    # Get Search Results
    test_endpoint "Get Search Results" "GET" "$RIDER_BASE_URL/rideSearch/$SEARCH_ID/results" \
      "token: $RIDER_TOKEN"
    
    ESTIMATE_ID=$(echo "$RESULTS" | jq -r '.tests[-1].responsePreview | fromjson? | .estimates[0].id // empty' 2>/dev/null || echo "")
    
    if [[ -n "$ESTIMATE_ID" ]]; then
      # Select Estimate
      test_endpoint "Select Estimate" "POST" "$RIDER_BASE_URL/estimate/$ESTIMATE_ID/select2" \
        "Content-Type: application/json token: $RIDER_TOKEN" \
        "{\"autoAssignEnabled\": true, \"autoAssignEnabledV2\": true}"
    fi
  fi
fi

# ============================================
# RIDE MANAGEMENT APIs
# ============================================
echo ""
echo "=========================================="
echo "RIDE MANAGEMENT APIs"
echo "=========================================="

if [[ -n "$DRIVER_TOKEN" ]]; then
  # Get Driver Rides
  test_endpoint "Get Driver Rides" "GET" "$DRIVER_BASE_URL/driver/ride/list?limit=5" \
    "token: $DRIVER_TOKEN"
fi

if [[ -n "$RIDER_TOKEN" ]]; then
  # Get Rider Bookings
  test_endpoint "Get Rider Active Bookings" "GET" "$RIDER_BASE_URL/rideBooking/list?onlyActive=true&limit=5" \
    "token: $RIDER_TOKEN"
fi

# ============================================
# CALL APIs
# ============================================
echo ""
echo "=========================================="
echo "CALL APIs"
echo "=========================================="

if [[ -n "$RIDER_TOKEN" ]]; then
  # Call Status (may return 400 without active call, but endpoint should exist)
  test_endpoint "Get Call Status" "GET" "$RIDER_BASE_URL/ride/call/status" \
    "token: $RIDER_TOKEN" \
    "" \
    "400"
fi

# ============================================
# CANCELLATION APIs
# ============================================
echo ""
echo "=========================================="
echo "CANCELLATION APIs"
echo "=========================================="

if [[ -n "$RIDER_TOKEN" ]]; then
  # Get Cancellation Reasons
  test_endpoint "Get Cancellation Reasons" "GET" "$RIDER_BASE_URL/cancellationReason?language=en" \
    "token: $RIDER_TOKEN"
fi

# ============================================
# RATING APIs
# ============================================
echo ""
echo "=========================================="
echo "RATING APIs"
echo "=========================================="

if [[ -n "$RIDER_TOKEN" ]]; then
  # Get Feedback Form
  test_endpoint "Get Feedback Form" "GET" "$RIDER_BASE_URL/feedbackForm?language=en" \
    "token: $RIDER_TOKEN"
fi

# ============================================
# MAPS APIs
# ============================================
echo ""
echo "=========================================="
echo "MAPS APIs"
echo "=========================================="

if [[ -n "$RIDER_TOKEN" ]]; then
  # Get Place Name
  test_endpoint "Get Place Name" "GET" "$RIDER_BASE_URL/maps/getPlaceName?lat=12.9352&lon=77.6245" \
    "token: $RIDER_TOKEN"
fi

# ============================================
# Finalize Results
# ============================================
RESULTS=$(echo "$RESULTS" | jq --arg end_time "$(date -Iseconds)" '.endTime = $end_time')

# Calculate summary
TOTAL_TESTS=$(echo "$RESULTS" | jq '.tests | length')
PASSED_TESTS=$(echo "$RESULTS" | jq '[.tests[] | select(.status == "PASSED")] | length')
FAILED_TESTS=$(echo "$RESULTS" | jq '[.tests[] | select(.status == "FAILED")] | length')

RESULTS=$(echo "$RESULTS" | jq \
  --argjson total "$TOTAL_TESTS" \
  --argjson passed "$PASSED_TESTS" \
  --argjson failed "$FAILED_TESTS" \
  '.summary = {totalTests: $total, passed: $passed, failed: $failed}')

# Save results
echo "$RESULTS" | jq '.' > "$RESULTS_FILE"

# Print summary
echo ""
echo "=========================================="
echo "TEST SUMMARY"
echo "=========================================="
echo "Total Tests: $TOTAL_TESTS"
echo "Passed: $PASSED_TESTS"
echo "Failed: $FAILED_TESTS"
echo "Results saved to: $RESULTS_FILE"
echo "=========================================="

if [[ $FAILED_TESTS -gt 0 ]]; then
  echo ""
  echo "Failed Tests:"
  echo "$RESULTS" | jq -r '.tests[] | select(.status == "FAILED") | "  - \(.name): HTTP \(.httpCode) (expected \(.expectedCode // "200"))"'
  exit 1
else
  echo ""
  echo "✓ All tests passed!"
  exit 0
fi
