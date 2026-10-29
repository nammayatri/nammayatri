# Journey ownership: rider B can neither read nor act on rider A's journey, and A's booking is untouched afterwards.
# Needs --variable token=<rider A token> --variable token_b=<rider B token> (run.sh journeyowner logs both in).

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END", "force": true}
HTTP *

POST {{base_url}}/internal/sharedCab/route/select
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "agencyId": "{{ibc_id}}", "serviceTierType": "SHARED_CAB", "capacity": 4, "routeCode": "{{route_a}}", "walkupCount": 0, "driverName": "{{driver_name}}", "vehicleModel": "{{vehicle_model}}"}
HTTP 200

# Rider A books a seat
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

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
[Options]
retry: 5
retry-interval: 2000ms
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "FINDING"

# Rider B, on A's journey: every journey-keyed endpoint refuses
GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token_b}}
HTTP 400
[Asserts]
body contains "journey does not belong to the caller"

POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setStatus/Completed
token: {{token_b}}
HTTP 400

POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setTrackingStatus/Finished
token: {{token_b}}
HTTP 400

POST {{base_url}}/v2/multimodal/{{journeyId}}/complete
token: {{token_b}}
HTTP 400

POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/softCancel
token: {{token_b}}
HTTP 400

POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/cancel
token: {{token_b}}
HTTP 400

# A's booking is still live, and A can still act on it
GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "FINDING"

POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/cancel
token: {{token}}
HTTP 200

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END", "force": true}
HTTP *
