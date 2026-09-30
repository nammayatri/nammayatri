# 6.8 rider flow: search without and with a cab, book with no payment order, cancel, book again, "I got down".
# Needs --variable token=<rider token from login.hurl>. Untested until a backend is up.
# Allocation is gated off, so a booking stays FINDING (vehicleNumber null) throughout.

# Clean start: no cab on the route
POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END", "force": true}
HTTP *

# No cab on the route: the shared-cab itinerary is hidden (05 decision 9)
POST {{base_url}}/v2/multimodalSearch
token: {{token}}
Content-Type: application/json
initateJourney: false
{"fareProductType": "ONE_WAY", "contents": {"origin": {"gps": {"lat": {{from_lat}}, "lon": {{from_lon}}}, "address": {"title": "board"}}, "destination": {"gps": {"lat": {{to_lat}}, "lon": {{to_lon}}}, "address": {"title": "drop"}}, "doMultimodalSearch": true, "stops": []}}
HTTP 200
[Asserts]
jsonpath "$.journeys[*].journeyLegs[*].routeDetails[?(@.routeCode == '{{route_a}}')]" not exists

# A cab goes ACTIVE on the route
POST {{base_url}}/internal/sharedCab/route/select
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "agencyId": "{{ibc_id}}", "serviceTierType": "SHARED_CAB", "capacity": 4, "routeCode": "{{route_a}}", "walkupCount": 0}
HTTP 200
[Asserts]
jsonpath "$.session.status" == "ACTIVE"

# With the cab, the itinerary appears
POST {{base_url}}/v2/multimodalSearch
token: {{token}}
Content-Type: application/json
initateJourney: false
{"fareProductType": "ONE_WAY", "contents": {"origin": {"gps": {"lat": {{from_lat}}, "lon": {{from_lon}}}, "address": {"title": "board"}}, "destination": {"gps": {"lat": {{to_lat}}, "lon": {{to_lon}}}, "address": {"title": "drop"}}, "doMultimodalSearch": true, "stops": []}}
HTTP 200
[Captures]
journeyId: jsonpath "$.journeys[0].journeyId"
[Asserts]
jsonpath "$.journeys[*].journeyLegs[*].routeDetails[?(@.routeCode == '{{route_a}}')]" exists

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
[Asserts]
jsonpath "$.legs[?(@.travelMode == 'SharedCab')].legExtraInfo.contents.routeCode" == "{{route_a}}"

# Pay on board: confirm opens no payment order
POST {{base_url}}/v2/multimodal/{{journeyId}}/confirm
token: {{token}}
Content-Type: application/json
{"enableOffer": false, "journeyConfirmReqElements": [{"journeyLegOrder": {{legOrder}}, "skipBooking": false, "categorySelectionReq": [{"quoteCategoryId": "{{categoryId}}", "quantity": 1}]}]}
HTTP 200
[Asserts]
jsonpath "$.orderSdkPayload" == null

GET {{base_url}}/v2/multimodal/{{journeyId}}/booking/paymentStatus
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.paymentOrder" == null

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
[Options]
retry: 5
retry-interval: 2000ms
HTTP 200
[Asserts]
jsonpath "$.journeyPaymentStatus" == null
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "FINDING"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.vehicleNumber" == null

# Cancel before boarding is allowed
POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/cancel
token: {{token}}
HTTP 200

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
[Options]
retry: 5
retry-interval: 2000ms
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "CANCELLED"

# Book again, then "I got down" before any boarding: the drop of an unboarded booking is a cancel (R54)
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
[Asserts]
jsonpath "$.orderSdkPayload" == null

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
[Options]
retry: 5
retry-interval: 2000ms
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "FINDING"

POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setStatus/Completed
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "CANCELLED"

# The cab leaves: the itinerary is hidden again
POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END"}
HTTP 200

POST {{base_url}}/v2/multimodalSearch
token: {{token}}
Content-Type: application/json
initateJourney: false
{"fareProductType": "ONE_WAY", "contents": {"origin": {"gps": {"lat": {{from_lat}}, "lon": {{from_lon}}}, "address": {"title": "board"}}, "destination": {"gps": {"lat": {{to_lat}}, "lon": {{to_lon}}}, "address": {"title": "drop"}}, "doMultimodalSearch": true, "stops": []}}
HTTP 200
[Asserts]
jsonpath "$.journeys[*].journeyLegs[*].routeDetails[?(@.routeCode == '{{route_a}}')]" not exists
