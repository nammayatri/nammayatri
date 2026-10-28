# A city whose only BUS-category config is the shared-cab feed has no bus fleet. The Share Ride screen asks for nearby buses on every
# visit (an empty list, not an error), and picking a stop asks which routes pass it (the cab routes, not an error).
# Needs --variable token=<rider token>.

POST {{base_url}}/v2/identifyNearByBus
token: {{token}}
Content-Type: application/json
{"city": "{{city}}", "riderLat": {{from_lat}}, "riderLon": {{from_lon}}}
HTTP 200
[Asserts]
jsonpath "$.buses" count == 0

# a stop picked on its own: the routes of the cab feed that pass it
POST {{base_url}}/v2/multimodal/routeServiceability
token: {{token}}
Content-Type: application/json
{"sourceStopCode": "{{stop_code}}"}
HTTP 200
[Asserts]
jsonpath "$.legs[0].routeWithLiveVehicles[*].routeCode" includes "{{stop_route}}"
