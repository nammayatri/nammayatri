# The Share Ride screen asks for nearby buses on every visit: no live bus is an empty list, not an error. Picking a stop asks which
# routes pass it; the cab feed answers that only in a city with no bus fleet, and this one (Chennai) runs chennai_data_bus.
# Needs --variable token=<rider token>.

POST {{base_url}}/v2/identifyNearByBus
token: {{token}}
Content-Type: application/json
{"city": "{{city}}", "riderLat": {{from_lat}}, "riderLon": {{from_lon}}}
HTTP 200
[Asserts]
jsonpath "$.buses" count == 0

# a stop picked on its own, in a city with a bus fleet: the bus path answers, never with the cab feed's routes
POST {{base_url}}/v2/multimodal/routeServiceability
token: {{token}}
Content-Type: application/json
{"sourceStopCode": "{{stop_code}}"}
HTTP *
[Asserts]
body not contains "{{stop_route}}"
