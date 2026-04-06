# Run Integration Tests with RCA

You are running NammaYatri integration tests and performing root cause analysis on failures.

## Test runner location
- Script: `Backend/dev/integration-tests/run-tests.sh`
- Collections: `Backend/dev/integration-tests/collections/`
- Process-compose logs: `*.log` files in the project root (`/Users/khuzemakhomosi/Documents/international/nammayatri/`)

## Available test commands
```
./run-tests.sh rides [CITY] [SUITE]           # Ride booking (NY_Bangalore, YS_Kolkata, NY_Chennai, BT_Delhi)
./run-tests.sh online [CITY] [SUITE]          # Online Stripe ride (BF_Helsinki)
./run-tests.sh online-offers [CITY] [SUITE]   # Online discount offers (BF_Helsinki)
./run-tests.sh offline-offers [CITY] [SUITE]  # Offline cashback offers (BT_Delhi)
./run-tests.sh bus [CITY] [SUITE]             # Bus tickets (FRFS_Chennai, FRFS_Bhubaneshwar)
./run-tests.sh metro [CITY] [SUITE]           # Metro tickets (FRFS_Bangalore, FRFS_Chennai)
./run-tests.sh subway [CITY] [SUITE]          # Subway tickets (FRFS_Chennai)
./run-tests.sh --list                         # List all suites
```

## Execution procedure

For EACH test collection+city combination, follow this exact procedure:

### Step 1: Clear logs
```bash
cd /Users/khuzemakhomosi/Documents/international/nammayatri
: > rider-app-exe.log
: > dynamic-offer-driver-app-exe.log
: > rider-app-scheduler-exe.log
: > driver-offer-allocator-exe.log
: > mock-server.log
: > location-tracking-service.log
: > search-result-aggregator-exe.log
: > producer-exe.log
: > kafka-consumers-exe.log
```

### Step 2: Run the test
```bash
cd Backend/dev/integration-tests
./run-tests.sh <collection> <city> [suite] -vp 2>&1
```
Use `-vp` (verbose pretty) to get full request/response JSON for failed APIs.

### Step 3: On failure — Capture RCA
If the test fails, immediately collect ALL relevant logs and perform detailed analysis.

#### 3a. Extract error logs from ALL services
For each service, extract errors that occurred DURING the test window (logs are clean since we cleared them in step 1). Use grep to find errors but also look for context around the error (use -B5 -A5 for surrounding lines).

```bash
ROOT="/Users/khuzemakhomosi/Documents/international/nammayatri"

echo "=== RIDER-APP ERRORS ==="
grep -n "ERROR\|error\|Exception\|FAIL\|400\|500\|not found\|NO_FARE\|not serviceable" "$ROOT/rider-app-exe.log" | grep -v "rdkafka\|kvdb" | tail -40

echo "=== DRIVER-APP ERRORS ==="
grep -n "ERROR\|error\|Exception\|FAIL\|no.*driver\|pool.*empty\|NO_FARE" "$ROOT/dynamic-offer-driver-app-exe.log" | grep -v "rdkafka\|kvdb" | tail -40

echo "=== MOCK-SERVER ERRORS ==="
grep -n "ERROR\|404\|500\|not found\|unhandled" "$ROOT/mock-server.log" | tail -20

echo "=== LOCATION-TRACKING-SERVICE ERRORS ==="
grep -n "error\|ERROR\|FAIL" "$ROOT/location-tracking-service.log" | tail -20

echo "=== SEARCH-RESULT-AGGREGATOR ERRORS ==="
grep -n "error\|ERROR" "$ROOT/search-result-aggregator-exe.log" | grep -v "rdkafka" | tail -10

echo "=== SCHEDULER ERRORS ==="
grep -n "error\|ERROR" "$ROOT/rider-app-scheduler-exe.log" | grep -v "rdkafka" | tail -10

echo "=== ALLOCATOR ERRORS ==="
grep -n "error\|ERROR" "$ROOT/driver-offer-allocator-exe.log" | grep -v "rdkafka" | tail -10
```

#### 3b. Trace the specific failing request
Based on the failing API from the test output, trace the full request lifecycle:

- **Search fails**: Look for the search request ID in rider-app logs, then trace it to BPP via the beckn transaction ID. Check if on_search callback was received.
- **Driver not found**: Check LTS logs for getNearbyDrivers calls, check driver_pool_config radius, check if driver was online and in correct geometry.
- **Payment fails**: Check mock-server for Stripe/Juspay request handling, check if the right payment config exists.
- **Estimate empty**: Check BPP logs for fare_product/fare_policy lookup, check if progressive_details exist for the fare_policy_id.
- **400 errors**: Extract the full error response body from the verbose test output — it usually contains the exact error message.
- **Station/route empty**: Check if FRFS config, stop_information, route data exist for the city.

#### 3c. Extract detailed context for the error
For the specific error found, get surrounding log lines to understand the full context:
```bash
# Find the error line number, then get context
grep -n "THE_ERROR_PATTERN" "$ROOT/service.log" | tail -1  # get line number
sed -n 'START,ENDp' "$ROOT/service.log"  # extract context window
```

### Step 4: Classify the failure
Categorize each failure as:
- **DATA**: Missing/wrong config data in master → fix in `Backend/dev/config-sync/assets/data/master/`
- **CODE**: Bug in Haskell service code → needs code fix
- **MOCK**: Mock server not handling a route/payload → fix in `Backend/dev/mock-servers/`
- **INFRA**: Service not running, port conflict, connection refused → process-compose issue
- **TEST**: Test assertion wrong or test data stale → fix in collection JSON

### Step 5: Report
After running all requested collections, produce a detailed report for EACH test:

```
## <Collection> / <City> / <Suite> — <PASS/FAIL>

**Failing Step**: <API name>
**Error**: <exact error from test output>
**Category**: <DATA/CODE/MOCK/INFRA/TEST>

### Error Logs

#### rider-app-exe
<paste relevant error lines with line numbers>

#### dynamic-offer-driver-app-exe
<paste relevant error lines with line numbers>

#### mock-server
<paste relevant error lines if any>

#### location-tracking-service
<paste relevant error lines if any>

### Detailed RCA
<Explain the full chain of events: what the test did, what each service did, where exactly it broke and why>

### Suggested Fix
<For DATA: which table/field to change in master data>
<For CODE: which file/function has the bug>
<For MOCK: which route/handler needs updating>
<For TEST: what assertion or test data to fix>
```

After all individual reports, produce a summary table:

| Collection | City | Suite | Status | Failing Step | Category | Root Cause (1-line) |
|---|---|---|---|---|---|---|

## Important notes
- Always clear logs BEFORE each test run so you only see logs from that specific test
- Run tests at the most granular level (specific suite) for clearer RCA
- The `rides` collection has both Auto and Cab suites — run them separately
- Bangalore only supports Auto (no Cab fare policies)
- Use `./run-tests.sh --check` to detect stuck bookings before running tests
- If tests are hanging, check if services are healthy in process-compose
