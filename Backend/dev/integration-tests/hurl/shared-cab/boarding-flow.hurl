# 8.7 boarding flow: allocate -> code -> drop, wrong cab -> re-bind, walk-up spot booking.
# Needs --variable token=<rider token from login.hurl>. Untested until 8.1 merges (boarding is on
# backend/feat/shared-cab-prime-81) and allocation (7.1-7.4) is on with sharedCabAllocationEnabled.
# Endpoints and errors are taken from the 8.1 branch: the code is the plate's last 4 on
# setOnboardedVehicleDetails; no match -> 400 SHARED_CAB_BOARDING_FAILED.
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
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "integratedBppConfigId": "{{ibc_id}}", "serviceTierType": "SHARED_CAB", "capacity": 4, "routeCode": "{{route_a}}", "walkupCount": 0}
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
legOrder: jsonpath "$.legs[?(@.travelMode == 'Bus')].order" nth 0
categoryId: jsonpath "$.legs[?(@.travelMode == 'Bus')].legExtraInfo.contents.categories[0].categoryId" nth 0

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
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" nth 0 == "ALLOCATED"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.vehicleNumber" nth 0 == "{{plate}}"

# The allocated seat already counts against the cab
GET {{base_url}}/v2/sharedCab/routes/{{route_a}}
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.cabs[?(@.plateLast4 == '{{plate_code}}')].freeSeats" nth 0 == 3

# A code that matches no cab on the route: one generic error (05 §8.1)
POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setOnboardedVehicleDetails
token: {{token}}
Content-Type: application/json
{"vehicleNumber": "0000", "supportsBoardingConfirmation": true}
HTTP 400
[Asserts]
jsonpath "$.errorCode" == "SHARED_CAB_BOARDING_FAILED"

# The sticker code boards: ticket INPROGRESS, leg BOARDED on the allocated cab
POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setOnboardedVehicleDetails
token: {{token}}
Content-Type: application/json
{"vehicleNumber": "{{plate_code}}", "supportsBoardingConfirmation": true}
HTTP 200
[Asserts]
jsonpath "$.boardingConfirmationRequired" not exists

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" nth 0 == "BOARDED"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.vehicleNumber" nth 0 == "{{plate}}"

# On board: cancel is refused (R7) and the driver can't end without force (04 §4)
POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/cancel
token: {{token}}
HTTP 400

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
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" nth 0 == "DROPPED"

GET {{base_url}}/v2/sharedCab/routes/{{route_a}}
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.cabs[?(@.plateLast4 == '{{plate_code}}')].freeSeats" nth 0 == 4

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
legOrder: jsonpath "$.legs[?(@.travelMode == 'Bus')].order" nth 0
categoryId: jsonpath "$.legs[?(@.travelMode == 'Bus')].legExtraInfo.contents.categories[0].categoryId" nth 0

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
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" nth 0 == "ALLOCATED"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.vehicleNumber" nth 0 == "{{plate}}"

POST {{base_url}}/internal/sharedCab/route/select
token: {{internal_key}}
{"driverId": "{{driver_b}}", "vehicleNumber": "{{plate_b}}", "integratedBppConfigId": "{{ibc_id}}", "serviceTierType": "SHARED_CAB", "capacity": 4, "routeCode": "{{route_a}}", "walkupCount": 0}
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
jsonpath "$.boardingConfirmationRequired" not exists

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" nth 0 == "BOARDED"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.vehicleNumber" nth 0 == "{{plate_b}}"

# A's seat freed itself; B holds it
GET {{base_url}}/v2/sharedCab/routes/{{route_a}}
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.cabs[?(@.plateLast4 == '{{plate_code}}')].freeSeats" nth 0 == 4
jsonpath "$.cabs[?(@.plateLast4 == '{{plate_b_code}}')].freeSeats" nth 0 == 3

POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setStatus/Completed
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" nth 0 == "DROPPED"

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver_b}}", "vehicleNumber": "{{plate_b}}", "next": "END"}
HTTP 200

# ---- 3. walk-up spot booking ----
# The rider walks up to cab A at the stop, books and types the code at once: the code boards a
# FINDING booking directly, or one the tick has just allocated to A, whichever comes first.

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
legOrder: jsonpath "$.legs[?(@.travelMode == 'Bus')].order" nth 0
categoryId: jsonpath "$.legs[?(@.travelMode == 'Bus')].legExtraInfo.contents.categories[0].categoryId" nth 0

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

POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setOnboardedVehicleDetails
token: {{token}}
Content-Type: application/json
{"vehicleNumber": "{{plate_code}}", "supportsBoardingConfirmation": true}
HTTP 200
[Asserts]
jsonpath "$.boardingConfirmationRequired" not exists

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" nth 0 == "BOARDED"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.vehicleNumber" nth 0 == "{{plate}}"

GET {{base_url}}/v2/sharedCab/routes/{{route_a}}
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.cabs[?(@.plateLast4 == '{{plate_code}}')].freeSeats" nth 0 == 3

POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setStatus/Completed
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" nth 0 == "DROPPED"

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END"}
HTTP 200
