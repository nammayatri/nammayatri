# Boarded on a trip: cab A on route_a, book, allocated, boarded with the sticker code. run.sh tripcheck then points the booking at another
# trip (a rider still marked on board from an earlier run of the plate) and runs call-chat-trip-after.hurl.
# Needs --variable token=<rider token>, call and chat on for the city.

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END", "force": true}
HTTP *

POST {{base_url}}/internal/sharedCab/route/select
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "agencyId": "{{ibc_id}}", "serviceTierType": "SHARED_CAB", "capacity": 4, "routeCode": "{{route_a}}", "walkupCount": 0, "driverName": "{{driver_name}}", "vehicleModel": "{{vehicle_model}}"}
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
[Captures]
bookingId: jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.bookingId"

POST {{base_url}}/v2/multimodal/{{journeyId}}/rider/location
token: {{token}}
Content-Type: application/json
{"latLong": {"lat": {{board_lat}}, "lon": {{board_lon}}}, "currTime": "{{newDate}}"}
HTTP 200

POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setOnboardedVehicleDetails
token: {{token}}
Content-Type: application/json
{"vehicleNumber": "{{plate_code}}", "supportsBoardingConfirmation": true}
HTTP 200
[Asserts]
jsonpath "$.boardingConfirmationRequired" == null

# boarded on this trip: the call and the chat are open
GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "BOARDED"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.maskedCallNumber" matches "^0?{{exophone}}$"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.chatChannelId" == "{{bookingId}}"

GET {{base_url}}/internal/sharedCab/booking/{{bookingId}}/callNumber
token: {{internal_key}}
[QueryStringParams]
driverId: {{driver}}
vehicleNumber: {{plate}}
HTTP 200
