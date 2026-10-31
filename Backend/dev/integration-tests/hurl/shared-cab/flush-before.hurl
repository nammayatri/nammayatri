# Flush recovery, part 1: leave an ACTIVE session with a walk-up for run.sh to flush from Redis. Part 2 is flush-after.hurl.

POST {{base_url}}/internal/sharedCab/route/operations
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "operation": "END_ROUTE", "forceEnd": true}
HTTP *

POST {{base_url}}/internal/sharedCab/route/operations
token: {{internal_key}}
{"operation": "SELECT_ROUTE", "driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "agencyId": "{{ibc_id}}", "serviceTierType": "SHARED_CAB", "capacity": 4, "routeCode": "{{route_a}}", "walkupCount": 1, "driverName": "{{driver_name}}", "vehicleModel": "{{vehicle_model}}"}
HTTP 200
[Asserts]
jsonpath "$.session.status" == "ACTIVE"
jsonpath "$.session.route.code" == "{{route_a}}"
jsonpath "$.session.walkupCount" == 1

GET {{base_url}}/internal/sharedCab/trips?driverId={{driver}}&date={{date}}
token: {{internal_key}}
[Options]
retry: 8
retry-interval: 1000ms
HTTP 200
[Asserts]
jsonpath "$.trips[-1:].status" == "ACTIVE"
