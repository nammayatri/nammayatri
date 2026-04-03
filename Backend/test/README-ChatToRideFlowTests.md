# Chat to Ride Booking Flow - API Tests

This directory contains comprehensive tests for the complete flow from chat/messaging to ride booking.

## Test Files

### 1. `chat-to-ride-flow-test.sh`
A complete end-to-end bash script that tests the entire flow:
- Driver Authentication (Auth + OTP)
- Driver Onboarding (via Dashboard)
- Driver Location & Online Status
- Rider Authentication
- Messaging APIs
- Call APIs
- Ride Search & Booking
- Ride Management (Start/End)

**Usage:**
```bash
# Make executable and run
chmod +x chat-to-ride-flow-test.sh
./chat-to-ride-flow-test.sh [environment]

# Environments: local (default), dev, staging
./chat-to-ride-flow-test.sh local
```

### 2. `api-endpoint-tests.sh`
Tests individual API endpoints with detailed JSON output:
- Authentication APIs
- Profile APIs
- Dashboard APIs
- Location Tracking APIs
- Driver Activity APIs
- Ride Search APIs
- Ride Management APIs
- Call APIs
- Cancellation APIs
- Rating APIs
- Maps APIs

**Usage:**
```bash
chmod +x api-endpoint-tests.sh
./api-endpoint-tests.sh
```

Results are saved to `results/api-test-results-YYYYMMDD-HHMMSS.json`

### 3. `ChatToRideBookingFlow.postman_collection.json`
A Postman collection with organized folders:
1. Authentication (Driver & Rider)
2. Driver Onboarding
3. Messaging & Chat APIs
4. Location & Activity
5. Ride Search & Booking
6. Driver Ride Management
7. Rider Ride Management
8. Call APIs
9. Cancellation APIs
10. Rating & Feedback
11. Maps & Location

**Usage:**
1. Import into Postman
2. Set up environment variables (see below)
3. Run collection

## Environment Variables

### For Local Development

Create a Postman environment with these variables:

```json
{
  "baseURL_namma_P": "http://localhost:8016/ui",
  "baseUrl_app": "http://localhost:8013/v2",
  "baseUrl_lts": "http://localhost:8081/ui",
  "baseURL_BPP_Dashboard": "http://localhost:8018/api",
  "baseURL_BPP_Dashboard_Internal": "http://localhost:8018/bpp/driver-offer",
  "dashboard_base_url": "http://localhost:8018",
  "dashboard_token": "local-admin-token-bangalore-namma-yatri",
  "dashboard_merchant_id": "NAMMA_YATRI_PARTNER",
  "dashboard_city": "Bangalore",
  "driver_merchant_id": "7f7896dd-787e-4a0b-8675-e9e6fe93bb8f",
  "bap_merchant_id": "4b17bd06-ae7e-48e9-85bf-282fb310209c",
  "city": "Bangalore",
  "state": "Karnataka",
  "login_otp": "7891",
  "origin_lat": "12.9352",
  "origin_lon": "77.6245",
  "dest_lat": "12.9716",
  "dest_lon": "77.6412"
}
```

## API Endpoints Tested

### Authentication APIs
| Endpoint | Method | Description |
|----------|--------|-------------|
| `/auth` | POST | Initiate authentication |
| `/auth/{authId}/verify` | POST | Verify OTP |

### Driver APIs
| Endpoint | Method | Description |
|----------|--------|-------------|
| `/driver/profile` | GET | Get driver profile |
| `/driver/stats` | GET | Get driver statistics |
| `/driver/setActivity` | POST | Set online/offline status |
| `/driver/nearbyRideRequest` | GET | Get nearby ride requests |
| `/driver/ride/list` | GET | List driver rides |
| `/driver/ride/{id}/start` | POST | Start a ride |
| `/driver/ride/{id}/end` | POST | End a ride |

### Messaging APIs
| Endpoint | Method | Description |
|----------|--------|-------------|
| `/message/list` | GET | List messages |
| `/message/{id}` | GET | Get message detail |
| `/message/{id}/seen` | PUT | Mark message as seen |
| `/message/{id}/like` | PUT | Like a message |
| `/message/{id}/response` | PUT | Reply to message |
| `/onMessage` | POST | Send FCM message |

### Rider APIs
| Endpoint | Method | Description |
|----------|--------|-------------|
| `/profile` | GET | Get rider profile |
| `/rideSearch` | POST | Create ride search |
| `/rideSearch/{id}/results` | GET | Get search results |
| `/estimate/{id}/select2` | POST | Select estimate |
| `/rideBooking/list` | GET | List bookings |
| `/rideBooking/{id}` | GET | Get booking details |

### Call APIs
| Endpoint | Method | Description |
|----------|--------|-------------|
| `/ride/{id}/call/driver` | POST | Call driver |
| `/ride/call/status` | GET | Get call status |

### Dashboard APIs
| Endpoint | Method | Description |
|----------|--------|-------------|
| `/user/switchMerchantAndCity` | POST | Switch city context |
| `/driver/{id}/addVehicle` | POST | Add vehicle to driver |
| `/driver/{id}/enable` | POST | Enable driver |

### Location Tracking APIs
| Endpoint | Method | Description |
|----------|--------|-------------|
| `/driver/location` | POST | Update driver location |

## Prerequisites

1. **Local Development Environment Running:**
   - Rider App (port 8013)
   - Driver App (port 8016)
   - Location Tracking Service (port 8081)
   - Provider Dashboard (port 8018)
   - PostgreSQL (port 5434)
   - Redis (port 6379)

2. **Test Data:**
   - Valid merchant IDs configured
   - Test OTP (default: 7891 for local)
   - Dashboard admin token

## Running Tests

### Option 1: Bash Script (Recommended for CI/CD)
```bash
cd Backend/test
./chat-to-ride-flow-test.sh local
```

### Option 2: Postman Collection
1. Import `ChatToRideBookingFlow.postman_collection.json`
2. Set up environment variables
3. Run collection with Collection Runner

### Option 3: Newman (CLI for Postman)
```bash
# Install newman
npm install -g newman

# Run collection
newman run ChatToRideBookingFlow.postman_collection.json \
  -e environment.json \
  --reporters cli,json \
  --reporter-json-export results.json
```

## Test Flow

```
1. Driver Auth (Auth → OTP Verify)
   ↓
2. Driver Onboarding (Dashboard: Add Vehicle → Enable)
   ↓
3. Driver Location (Set Location → Set Online)
   ↓
4. Rider Auth (Auth → OTP Verify)
   ↓
5. Messaging (Get Messages - optional)
   ↓
6. Ride Search (Create Search → Get Results → Select Estimate)
   ↓
7. Driver Accepts (Get Nearby Requests → Accept Ride)
   ↓
8. Booking Created (Rider gets booking, Driver gets ride)
   ↓
9. Ride Started (Driver starts ride with OTP)
   ↓
10. Ride Ended (Driver ends ride at destination)
   ↓
11. Post-Ride (Rating, Feedback, etc.)
```

## Expected Results

All tests should return HTTP 200 status codes. The scripts validate:
- Response status codes
- Required fields in responses
- Data types and formats
- Authentication token extraction

## Troubleshooting

### Common Issues

1. **Connection Refused**
   - Ensure all services are running
   - Check port configurations

2. **Authentication Failures**
   - Verify OTP configuration
   - Check merchant IDs

3. **No Ride Requests Found**
   - Ensure driver is online and location is set
   - Check that driver vehicle is added and enabled
   - Verify search location is within serviceable area

4. **Dashboard API Failures**
   - Verify dashboard token
   - Check merchant/city configuration

### Debug Mode

For detailed output, run with verbose flag:
```bash
./chat-to-ride-flow-test.sh local 2>&1 | tee test-output.log
```

## Integration with CI/CD

Add to your CI pipeline:

```yaml
# Example GitHub Actions
- name: Run API Tests
  run: |
    cd Backend/test
    ./chat-to-ride-flow-test.sh local
  continue-on-error: false
```

## Contributing

When adding new tests:
1. Follow existing naming conventions
2. Add tests to appropriate folder in Postman collection
3. Update this README with new endpoints
4. Ensure tests are idempotent (use random data)
