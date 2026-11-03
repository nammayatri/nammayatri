# Suspended-city, after rider_config.shared_cab_enabled was flipped to false (and the config caches cleared).
# Needs token, journeyId, legOrder, journeyId2, legOrder2, categoryId2 from suspended-city-before.hurl.
# Intake is refused or hidden; the booking that was already FINDING still times out to CANCELLED and is refunded in full.

# search: no cab itinerary although the cab is still running
POST {{base_url}}/v2/multimodalSearch
token: {{token}}
Content-Type: application/json
initateJourney: false
[Options]
retry: 5
retry-interval: 1000ms
{"fareProductType": "ONE_WAY", "contents": {"origin": {"gps": {"lat": {{from_lat}}, "lon": {{from_lon}}}, "address": {"title": "board"}}, "destination": {"gps": {"lat": {{to_lat}}, "lon": {{to_lon}}}, "address": {"title": "drop"}}, "doMultimodalSearch": true, "stops": []}}
HTTP 200
[Asserts]
jsonpath "$.journeys[*].journeyLegs[*].routeDetails[?(@.routeCode == '{{route_a}}')]" not exists

# confirm: refused with the suspended error, no booking is created
POST {{base_url}}/v2/multimodal/{{journeyId2}}/confirm
token: {{token}}
Content-Type: application/json
{"enableOffer": false, "journeyConfirmReqElements": [{"journeyLegOrder": {{legOrder2}}, "skipBooking": false, "categorySelectionReq": [{"quoteCategoryId": "{{categoryId2}}", "quantity": 1}]}]}
HTTP 400
[Asserts]
body contains "suspended"

# walk-up by the live plate: refused
GET {{base_url}}/v2/publicTransport/vehicleData/%22BUS%22/{{plate}}
token: {{token}}
HTTP 400
[Asserts]
body contains "suspended"

# the route list is empty
GET {{base_url}}/v2/sharedCab/routes
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.routes" count == 0

# the earlier FINDING booking still times out and is cancelled by the draining tick (cancelReason NO_CAB_FOUND)
GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
[Options]
retry: 40
retry-interval: 3000ms
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "CANCELLED"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.cancelReason" == "NO_CAB_FOUND"

# the cab leaves
POST {{base_url}}/internal/sharedCab/route/operations
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "operation": "END_ROUTE", "forceEnd": true}
HTTP *
