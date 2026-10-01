# 8.7 boarding flow: allocate -> code -> drop, wrong cab -> re-bind, walk-up spot booking.
# Needs --variable token=<rider token from login.hurl>. Untested until 8.1 merges (boarding is on
# backend/feat/shared-cab-prime-81) and allocation (7.1-7.4) is on with sharedCabAllocationEnabled.
# Endpoints and errors are taken from the 8.1 branch: the code is the plate's last 4 on
# setOnboardedVehicleDetails; no match -> 200 boardingConfirmationRequired (UNLISTED_CAB).
# No LTS pings here, so the proximity anchor is the board stop: the rider posts a location at from_lat/from_lon.

# Clean start: no cab on the route
POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END", "force": true}
HTTP *

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver_b}}", "vehicleNumber": "{{plate_b}}", "next": "END", "force": true}
HTTP *

# ---- 1. allocate -> code -> drop ----

POST {{base_url}}/internal/sharedCab/route/select
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "agencyId": "{{ibc_id}}", "serviceTierType": "SHARED_CAB", "capacity": 4, "routeCode": "{{route_a}}", "walkupCount": 0}
HTTP 200
[Asserts]
jsonpath "$.session.status" == "ACTIVE"

POST {{base_url}}/v2/multimodalSearch
token: {{token}}
Content-Type: application/json
initateJourney: false
{"fareProductType": "ONE_WAY", "contents": {"origin": {"gps": {"lat": {{from_lat}}, "lon": {{from_lon}}}, "address": {"title": "board"}}, "destination": {"gps": {"lat": {{to_lat}}, "lon": {{to_lon}}}, "address": {"title": "drop"}}, "doMultimodalSearch": true, "stops": []}}
HTTP 200
[Captures]
journeyId: jsonpath "$.journeys[0].journeyId"

POST {{base_url}}/v2/multimodal/{{journeyId}}/initiate
token: {{token}}
Content-Type: application/json
[Options]
retry: 10
retry-interval: 2000ms
{}
HTTP 200
[Captures]
legOrder: jsonpath "$.legs[?(@.travelMode == 'SharedCab')].order"
categoryId: jsonpath "$.legs[?(@.travelMode == 'SharedCab')].legExtraInfo.contents.categories[0].categoryId"

POST {{base_url}}/v2/multimodal/{{journeyId}}/confirm
token: {{token}}
Content-Type: application/json
{"enableOffer": false, "journeyConfirmReqElements": [{"journeyLegOrder": {{legOrder}}, "skipBooking": false, "categorySelectionReq": [{"quoteCategoryId": "{{categoryId}}", "quantity": 1}]}]}
HTTP 200

# At the stop: the tick allocates now (05 §3 "rider within atStopRadiusM")
POST {{base_url}}/v2/multimodal/{{journeyId}}/rider/location
token: {{token}}
Content-Type: application/json
{"latLong": {"lat": {{from_lat}}, "lon": {{from_lon}}}, "currTime": "{{newDate}}"}
HTTP 200

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
[Options]
retry: 10
retry-interval: 3000ms
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" matches "^(ALLOCATED|ARRIVING)$"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.vehicleNumber" == "{{plate}}"
# the pinger keeps the cab's LTS fix fresh: its position rides on the leg status
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.position.lat" isFloat
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.position.lon" isFloat

# The allocated seat already counts against the cab
GET {{base_url}}/v2/sharedCab/routes/{{route_a}}
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.cabs[?(@.plateLast4 == '{{plate_code}}')].freeSeats" == 3

# A code that matches no cab on the route: the rider is asked to confirm an unlisted cab (retry with forceCheckIn boards degraded)
POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setOnboardedVehicleDetails
token: {{token}}
Content-Type: application/json
{"vehicleNumber": "0000", "supportsBoardingConfirmation": true}
HTTP 200
[Asserts]
jsonpath "$.boardingConfirmationRequired" == true
jsonpath "$.boardingConfirmationReason" == "UNLISTED_CAB"

# The sticker code boards: ticket INPROGRESS, leg BOARDED on the allocated cab
POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setOnboardedVehicleDetails
token: {{token}}
Content-Type: application/json
{"vehicleNumber": "{{plate_code}}", "supportsBoardingConfirmation": true}
HTTP 200
[Asserts]
jsonpath "$.boardingConfirmationRequired" == null

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "BOARDED"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.vehicleNumber" == "{{plate}}"

# On board: cancel is refused (R7) and the driver can't end without force (04 §4)
POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/cancel
token: {{token}}
HTTP 409
[Asserts]
jsonpath "$.errorCode" == "SHARED_CAB_RIDE_STARTED"

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END"}
HTTP 409
[Asserts]
jsonpath "$.errorCode" == "SHARED_CAB_RIDERS_ON_BOARD"

# "I got down": DROPPED, and the seat is free again
POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setStatus/Completed
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "DROPPED"

GET {{base_url}}/v2/sharedCab/routes/{{route_a}}
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.cabs[?(@.plateLast4 == '{{plate_code}}')].freeSeats" == 4

# ---- 2. wrong cab -> re-bind ----
# Allocate to cab A while it is the only cab, then bring cab B on and board B with B's code.

POST {{base_url}}/v2/multimodalSearch
token: {{token}}
Content-Type: application/json
initateJourney: false
{"fareProductType": "ONE_WAY", "contents": {"origin": {"gps": {"lat": {{from_lat}}, "lon": {{from_lon}}}, "address": {"title": "board"}}, "destination": {"gps": {"lat": {{to_lat}}, "lon": {{to_lon}}}, "address": {"title": "drop"}}, "doMultimodalSearch": true, "stops": []}}
HTTP 200
[Captures]
journeyId: jsonpath "$.journeys[0].journeyId"

POST {{base_url}}/v2/multimodal/{{journeyId}}/initiate
token: {{token}}
Content-Type: application/json
[Options]
retry: 10
retry-interval: 2000ms
{}
HTTP 200
[Captures]
legOrder: jsonpath "$.legs[?(@.travelMode == 'SharedCab')].order"
categoryId: jsonpath "$.legs[?(@.travelMode == 'SharedCab')].legExtraInfo.contents.categories[0].categoryId"

POST {{base_url}}/v2/multimodal/{{journeyId}}/confirm
token: {{token}}
Content-Type: application/json
{"enableOffer": false, "journeyConfirmReqElements": [{"journeyLegOrder": {{legOrder}}, "skipBooking": false, "categorySelectionReq": [{"quoteCategoryId": "{{categoryId}}", "quantity": 1}]}]}
HTTP 200

POST {{base_url}}/v2/multimodal/{{journeyId}}/rider/location
token: {{token}}
Content-Type: application/json
{"latLong": {"lat": {{from_lat}}, "lon": {{from_lon}}}, "currTime": "{{newDate}}"}
HTTP 200

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
[Options]
retry: 10
retry-interval: 3000ms
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" matches "^(ALLOCATED|ARRIVING)$"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.vehicleNumber" == "{{plate}}"

POST {{base_url}}/internal/sharedCab/route/select
token: {{internal_key}}
{"driverId": "{{driver_b}}", "vehicleNumber": "{{plate_b}}", "agencyId": "{{ibc_id}}", "serviceTierType": "SHARED_CAB", "capacity": 4, "routeCode": "{{route_a}}", "walkupCount": 0}
HTTP 200
[Asserts]
jsonpath "$.session.status" == "ACTIVE"

# The rider gets into B: re-bound under B's lock, no penalty for A (05 §4, PRD §11.3)
POST {{base_url}}/v2/multimodal/{{journeyId}}/rider/location
token: {{token}}
Content-Type: application/json
{"latLong": {"lat": {{from_lat}}, "lon": {{from_lon}}}, "currTime": "{{newDate}}"}
HTTP 200

POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setOnboardedVehicleDetails
token: {{token}}
Content-Type: application/json
{"vehicleNumber": "{{plate_b_code}}", "supportsBoardingConfirmation": true}
HTTP 200
[Asserts]
jsonpath "$.boardingConfirmationRequired" == null

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "BOARDED"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.vehicleNumber" == "{{plate_b}}"

# A's seat freed itself; B holds it
GET {{base_url}}/v2/sharedCab/routes/{{route_a}}
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.cabs[?(@.plateLast4 == '{{plate_code}}')].freeSeats" == 4
jsonpath "$.cabs[?(@.plateLast4 == '{{plate_b_code}}')].freeSeats" == 3

POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setStatus/Completed
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "DROPPED"

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver_b}}", "vehicleNumber": "{{plate_b}}", "next": "END"}
HTTP 200

# ---- 3. walk-up: book then code ----
# The rider walks up to cab A at the stop. Scanning or typing its plate resolves the cab's route (8.4),
# then the rider books and types the code at once: the code boards a FINDING booking directly, or one
# the tick has just allocated to A, whichever comes first (05 §4; one-step spot booking was dropped).

# NOTE: the enum path segment is JSON-quoted (mkHttpInstancesForEnum): /BUS/ is a router-level 404, /%22BUS%22/ matches.
GET {{base_url}}/v2/publicTransport/vehicleData/%22BUS%22/{{plate}}
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.rs[0].st" == "SHARED_CAB"
jsonpath "$.rs[0].cd" == "{{route_a}}"

# The sticker code (plate's last 4) resolves among the city's live cabs too
GET {{base_url}}/v2/publicTransport/vehicleData/%22BUS%22/{{plate_code}}
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.rs[0].st" == "SHARED_CAB"
jsonpath "$.rs[0].cd" == "{{route_a}}"

POST {{base_url}}/v2/multimodalSearch
token: {{token}}
Content-Type: application/json
initateJourney: false
{"fareProductType": "ONE_WAY", "contents": {"origin": {"gps": {"lat": {{from_lat}}, "lon": {{from_lon}}}, "address": {"title": "board"}}, "destination": {"gps": {"lat": {{to_lat}}, "lon": {{to_lon}}}, "address": {"title": "drop"}}, "doMultimodalSearch": true, "stops": []}}
HTTP 200
[Captures]
journeyId: jsonpath "$.journeys[0].journeyId"

POST {{base_url}}/v2/multimodal/{{journeyId}}/initiate
token: {{token}}
Content-Type: application/json
[Options]
retry: 10
retry-interval: 2000ms
{}
HTTP 200
[Captures]
legOrder: jsonpath "$.legs[?(@.travelMode == 'SharedCab')].order"
categoryId: jsonpath "$.legs[?(@.travelMode == 'SharedCab')].legExtraInfo.contents.categories[0].categoryId"

POST {{base_url}}/v2/multimodal/{{journeyId}}/confirm
token: {{token}}
Content-Type: application/json
{"enableOffer": false, "journeyConfirmReqElements": [{"journeyLegOrder": {{legOrder}}, "skipBooking": false, "categorySelectionReq": [{"quoteCategoryId": "{{categoryId}}", "quantity": 1}]}]}
HTTP 200

# The rider's fix at the stop lets the tick allocate the booking while the code request is in flight: the code must board either way (B12).
POST {{base_url}}/v2/multimodal/{{journeyId}}/rider/location
token: {{token}}
Content-Type: application/json
{"latLong": {"lat": {{from_lat}}, "lon": {{from_lon}}}, "currTime": "{{newDate}}"}
HTTP 200

POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setOnboardedVehicleDetails
token: {{token}}
Content-Type: application/json
{"vehicleNumber": "{{plate_code}}", "supportsBoardingConfirmation": true}
HTTP 200
[Asserts]
jsonpath "$.boardingConfirmationRequired" == null

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "BOARDED"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.vehicleNumber" == "{{plate}}"

GET {{base_url}}/v2/sharedCab/routes/{{route_a}}
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.cabs[?(@.plateLast4 == '{{plate_code}}')].freeSeats" == 3

POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setStatus/Completed
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "DROPPED"

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END"}
HTTP 200
