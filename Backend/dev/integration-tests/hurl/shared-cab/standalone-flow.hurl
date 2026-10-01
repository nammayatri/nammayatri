# Standalone (non-journey) shared-cab booking on the pay-on-board tier: search -> quote -> confirm.
# No payment order is created, the booking confirms inline and its ticket is issued at once.
# Needs --variable token=<rider token from login.hurl>. The stops are the route_a seed stops (seed/02-geometry.sql).

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

# the enum query value is JSON-quoted (mkHttpInstancesForEnum), as on the vehicleData path
POST {{base_url}}/v2/frfs/search?vehicleType=%22BUS%22
token: {{token}}
Content-Type: application/json
{"fromStationCode": "SC_IEWDUH", "toStationCode": "SC_MAWLAI", "quantity": 1, "routeCode": "{{route_a}}", "platformType": "APPLICATION", "vehicleNumber": "{{plate}}"}
HTTP 200
[Captures]
searchId: jsonpath "$.searchId"

GET {{base_url}}/v2/frfs/search/{{searchId}}/quote?hasPasses=false
token: {{token}}
[Options]
retry: 10
retry-interval: 1000ms
HTTP 200
[Asserts]
jsonpath "$[0].routeCode" == "{{route_a}}"
[Captures]
quoteId: jsonpath "$[0].quoteId"
categoryId: jsonpath "$[0].categories[0].categoryId"

POST {{base_url}}/v2/frfs/quote/v2/{{quoteId}}/confirm?isMockPayment=false
token: {{token}}
Content-Type: application/json
{"categorySelectionReq": [{"quoteCategoryId": "{{categoryId}}", "quantity": 1}]}
HTTP 200
[Asserts]
jsonpath "$.status" == "CONFIRMED"
jsonpath "$.payment" == null
jsonpath "$.vehicleType" == "BUS"
[Captures]
bookingId: jsonpath "$.bookingId"

# the ticket is issued with the confirm; still no payment on the booking
GET {{base_url}}/v2/frfs/booking/{{bookingId}}/status
token: {{token}}
[Options]
retry: 10
retry-interval: 1000ms
HTTP 200
[Asserts]
jsonpath "$.status" == "CONFIRMED"
jsonpath "$.payment" == null
jsonpath "$.tickets[0].status" == "ACTIVE"

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END", "force": true}
HTTP *
