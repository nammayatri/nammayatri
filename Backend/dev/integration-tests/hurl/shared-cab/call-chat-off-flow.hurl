# The per-city switch: with shared_cab_call_enabled and shared_cab_chat_enabled off, an allocated leg carries neither the ExoPhone nor
# a chat channel, the driver's rider row is not callable, and rider-app refuses to hand out the rider's number.
# Needs --variable token=<rider token>; run by run.sh calloff after CALL_OFF_CMD (switch off, caches refreshed).

POST {{base_url}}/internal/sharedCab/route/operations
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "operation": "END_ROUTE", "forceEnd": true}
HTTP *

POST {{base_url}}/internal/sharedCab/route/operations
token: {{internal_key}}
{"operation": "SELECT_ROUTE", "driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "agencyId": "{{ibc_id}}", "serviceTierType": "SHARED_CAB", "capacity": 4, "routeCode": "{{route_a}}", "walkupCount": 0, "driverName": "{{driver_name}}", "vehicleModel": "{{vehicle_model}}"}
HTTP 200

POST {{base_url}}/v2/multimodalSearch
token: {{token}}
Content-Type: application/json
initateJourney: false
[Options]
retry: 5
retry-interval: 1000ms
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
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "ALLOCATED"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.maskedCallNumber" == null
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.chatChannelId" == null
[Captures]
bookingId: jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.bookingId"

GET {{base_url}}/internal/sharedCab/session
token: {{internal_key}}
[QueryStringParams]
driverId: {{driver}}
vehicleNumber: {{plate}}
HTTP 200
[Asserts]
jsonpath "$.ridersByStop[*].boarding[?(@.bookingId == '{{bookingId}}')].callable" == false
jsonpath "$.ridersByStop[*].boarding[?(@.bookingId == '{{bookingId}}')].chatChannelId" == null

GET {{base_url}}/internal/sharedCab/booking/{{bookingId}}/callNumber
token: {{internal_key}}
[QueryStringParams]
driverId: {{driver}}
vehicleNumber: {{plate}}
HTTP 403
[Asserts]
jsonpath "$.errorCode" == "SHARED_CAB_CALL_DISABLED"

POST {{base_url}}/internal/sharedCab/booking/{{bookingId}}/message
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "message": "hello"}
HTTP 403
[Asserts]
jsonpath "$.errorCode" == "SHARED_CAB_CHAT_DISABLED"

POST {{base_url}}/internal/sharedCab/route/operations
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "operation": "END_ROUTE", "forceEnd": true}
HTTP *
