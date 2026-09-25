# 3.6 flush recovery, part 1: leave an ACTIVE session with a walk-up for run.sh to flush from Redis.
# Untested until a backend is up. Part 2 is flush-after.hurl.

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END", "force": true}
HTTP *

POST {{base_url}}/internal/sharedCab/route/select
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "integratedBppConfigId": "{{ibc_id}}", "serviceTierType": "SHARED_CAB", "capacity": 4, "routeCode": "{{route_a}}", "walkupCount": 1}
HTTP 200
[Asserts]
jsonpath "$.session.status" == "ACTIVE"
jsonpath "$.session.route.code" == "{{route_a}}"
jsonpath "$.session.walkupCount" == 1

GET {{base_url}}/internal/sharedCab/trips?driverId={{driver}}&date={{date}}
token: {{internal_key}}
HTTP 200
[Asserts]
jsonpath "$.trips[-1:].status" nth 0 == "ACTIVE"
