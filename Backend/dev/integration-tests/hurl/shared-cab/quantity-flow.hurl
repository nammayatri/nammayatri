# Two riders on one booking (the confirm screen's rider-count stepper): the seats are held together,
# one code boards the pair, a drop frees both, a skip frees both, and a cab with a single free seat is not allocated to a pair.
# Needs --variable token=<rider token>. Cab A (capacity 4) on route_a; the pinger keeps its LTS fix fresh.
# The direct seller issues ONE group ticket for quantity 2; a booking's seats are its party size (Booking.seatWeight).

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END", "force": true}
HTTP *

POST {{base_url}}/internal/sharedCab/route/select
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "agencyId": "{{ibc_id}}", "serviceTierType": "SHARED_CAB", "capacity": 4, "routeCode": "{{route_a}}", "walkupCount": 0}
HTTP 200
[Asserts]
jsonpath "$.session.status" == "ACTIVE"

# ---- 1. a pair is allocated, boarded with one code and dropped ----
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
{"enableOffer": false, "journeyConfirmReqElements": [{"journeyLegOrder": {{legOrder}}, "skipBooking": false, "categorySelectionReq": [{"quoteCategoryId": "{{categoryId}}", "quantity": 2}]}]}
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

# both seats are held against the cab
GET {{base_url}}/v2/sharedCab/routes/{{route_a}}
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.cabs[?(@.plateLast4 == '{{plate_code}}')].freeSeats" == 2

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

GET {{base_url}}/v2/sharedCab/routes/{{route_a}}
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.cabs[?(@.plateLast4 == '{{plate_code}}')].freeSeats" == 2

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

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END", "force": true}
HTTP *

# ---- 2. a cab with one free seat is not allocated to a pair ----
POST {{base_url}}/internal/sharedCab/route/select
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "agencyId": "{{ibc_id}}", "serviceTierType": "SHARED_CAB", "capacity": 4, "routeCode": "{{route_a}}", "walkupCount": 3}
HTTP 200

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
{"enableOffer": false, "journeyConfirmReqElements": [{"journeyLegOrder": {{legOrder}}, "skipBooking": false, "categorySelectionReq": [{"quoteCategoryId": "{{categoryId}}", "quantity": 2}]}]}
HTTP 200

POST {{base_url}}/v2/multimodal/{{journeyId}}/rider/location
token: {{token}}
Content-Type: application/json
{"latLong": {"lat": {{from_lat}}, "lon": {{from_lon}}}, "currTime": "{{newDate}}"}
HTTP 200

# the tick has had time to allocate: it must not, the cab has one seat
GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
[Options]
delay: 12000ms
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "FINDING"

POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/cancel
token: {{token}}
HTTP 200

# a cancelled only-cab journey is over, not live
GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
[Options]
retry: 6
retry-interval: 1000ms
HTTP 200
[Asserts]
jsonpath "$.journeyStatus" == "CANCELLED"

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END", "force": true}
HTTP *

# ---- 3. a skip of a pair frees both seats ----
POST {{base_url}}/internal/sharedCab/route/select
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "agencyId": "{{ibc_id}}", "serviceTierType": "SHARED_CAB", "capacity": 4, "routeCode": "{{route_a}}", "walkupCount": 0}
HTTP 200

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
{"enableOffer": false, "journeyConfirmReqElements": [{"journeyLegOrder": {{legOrder}}, "skipBooking": false, "categorySelectionReq": [{"quoteCategoryId": "{{categoryId}}", "quantity": 2}]}]}
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
[Captures]
bookingId: jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.bookingId"

POST {{base_url}}/v2/sharedCab/booking/{{bookingId}}/skip
token: {{token}}
Content-Type: application/json
{"reason": "OTHER"}
HTTP 200

GET {{base_url}}/v2/sharedCab/routes/{{route_a}}
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.cabs[?(@.plateLast4 == '{{plate_code}}')].freeSeats" == 4

POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/cancel
token: {{token}}
HTTP 200

# a cancelled only-cab journey is over, not live
GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
[Options]
retry: 6
retry-interval: 1000ms
HTTP 200
[Asserts]
jsonpath "$.journeyStatus" == "CANCELLED"

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END", "force": true}
HTTP *
