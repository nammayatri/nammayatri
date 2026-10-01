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

# the shuttle map's route read: a cab route has no scheduled trip, so its stops and shape come from the stop mapping, in order
GET {{base_url}}/v2/frfs/route/{{route_a}}?city=std:044&vehicleType=%22BUS%22&platformType=%22APPLICATION%22
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.stops" count == 4
jsonpath "$.totalStops" == 4
jsonpath "$.stops[0].code" == "SC_IEWDUH"
jsonpath "$.stops[3].code" == "SC_MAWLAI"
jsonpath "$.stops[0].lat" isFloat
jsonpath "$.waypoints" count == 4
jsonpath "$.waypoints[0].lat" isFloat

# release the seat the booking holds, so later flows start from an empty cab
POST {{base_url}}/v2/frfs/booking/{{bookingId}}/cancel
token: {{token}}
HTTP 200

# opening the cab route from search: route serviceability answers from the cabs live on it (its own feed, not the bus one)
POST {{base_url}}/v2/multimodal/routeServiceability?allPassingRoutes=false
token: {{token}}
Content-Type: application/json
[Options]
retry: 8
retry-interval: 2000ms
{"routeCodes": [{"legOrder": 0, "routeCodes": ["{{route_a}}"]}]}
HTTP 200
[Asserts]
jsonpath "$.legs[0].routeWithLiveVehicles[0].liveVehicles[0].number" == "{{plate}}"
jsonpath "$.legs[0].routeWithLiveVehicles[0].liveVehicles[0].serviceTierType" == "SHARED_CAB"
jsonpath "$.legs[0].routeWithLiveVehicles[0].liveVehicles[0].position.lat" isFloat

# and narrowed to the booked cab by its plate (the tracking screen's call)
POST {{base_url}}/v2/multimodal/routeServiceability?allPassingRoutes=false
token: {{token}}
Content-Type: application/json
{"routeCodes": [{"legOrder": 0, "routeCodes": ["{{route_a}}"]}], "vehicleNumber": "{{plate}}"}
HTTP 200
[Asserts]
jsonpath "$.legs[0].routeWithLiveVehicles[0].liveVehicles[0].number" == "{{plate}}"

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END", "force": true}
HTTP *
