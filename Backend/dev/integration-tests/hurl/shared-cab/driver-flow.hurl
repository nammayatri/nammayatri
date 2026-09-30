# 3.6 driver flow: select -> walk-up toggle -> change route -> return -> end, on rider-app's internal
# /sharedCab APIs (what driver-app proxies). Untested until a backend is up.
# Needs --variable date=<today in IST, YYYY-MM-DD> (run.sh sets it). Ends with the session ENDED, so it re-runs.

# Clean start: end whatever a previous run left (404 when there is nothing to end)
POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END", "force": true}
HTTP *

# Select opens an ACTIVE session with no walk-ups: every seat free
POST {{base_url}}/internal/sharedCab/route/select
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "agencyId": "{{ibc_id}}", "serviceTierType": "SHARED_CAB", "capacity": 4, "routeCode": "{{route_a}}", "walkupCount": 0}
HTTP 200
[Captures]
v_open: jsonpath "$.session.version"
[Asserts]
jsonpath "$.session.status" == "ACTIVE"
jsonpath "$.session.route.code" == "{{route_a}}"
jsonpath "$.session.queuedRoute" == null
jsonpath "$.session.capacity" == 4
jsonpath "$.session.walkupCount" == 0
jsonpath "$.session.available" == 4
jsonpath "$.affectedRiders" == null

GET {{base_url}}/internal/sharedCab/trips?driverId={{driver}}&date={{date}}
token: {{internal_key}}
[Options]
retry: 8
retry-interval: 1000ms
HTTP 200
[Asserts]
jsonpath "$.trips[-1:].routeCode" == "{{route_a}}"
jsonpath "$.trips[-1:].status" == "ACTIVE"

# Walk-up toggled on: one seat fewer, version moves
POST {{base_url}}/internal/sharedCab/seats
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "walkupCount": 1, "version": {{v_open}}}
HTTP 200
[Captures]
v_walkup: jsonpath "$.version"
[Asserts]
jsonpath "$.status" == "ACTIVE"
jsonpath "$.walkupCount" == 1
jsonpath "$.available" == 3
jsonpath "$.version" > {{v_open}}

# The pre-toggle version is stale now
POST {{base_url}}/internal/sharedCab/seats
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "walkupCount": 0, "version": {{v_open}}}
HTTP 409
[Asserts]
jsonpath "$.errorCode" == "SHARED_CAB_SESSION_VERSION_MISMATCH"

# Walk-up toggled off with the current version
POST {{base_url}}/internal/sharedCab/seats
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "walkupCount": 0, "version": {{v_walkup}}}
HTTP 200
[Captures]
v_seats: jsonpath "$.version"
[Asserts]
jsonpath "$.walkupCount" == 0
jsonpath "$.available" == 4
jsonpath "$.version" > {{v_walkup}}

# Change route with nobody on board: applied at once, the old run closes as ROUTE_CHANGED
POST {{base_url}}/internal/sharedCab/route/select
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "agencyId": "{{ibc_id}}", "serviceTierType": "SHARED_CAB", "routeCode": "{{route_b}}", "walkupCount": 0}
HTTP 200
[Captures]
v_changed: jsonpath "$.session.version"
[Asserts]
jsonpath "$.session.status" == "ACTIVE"
jsonpath "$.session.route.code" == "{{route_b}}"
jsonpath "$.session.queuedRoute" == null
jsonpath "$.session.version" > {{v_seats}}

GET {{base_url}}/internal/sharedCab/trips?driverId={{driver}}&date={{date}}
token: {{internal_key}}
[Options]
retry: 8
retry-interval: 1000ms
HTTP 200
[Asserts]
jsonpath "$.trips[-2:].routeCode" nth 0 == "{{route_a}}"
jsonpath "$.trips[-2:].status" nth 0 == "COMPLETED"
jsonpath "$.trips[-2:].endReason" nth 0 == "ROUTE_CHANGED"
jsonpath "$.trips[-1:].routeCode" == "{{route_b}}"
jsonpath "$.trips[-1:].status" == "ACTIVE"

# GET session reflects the same state
GET {{base_url}}/internal/sharedCab/session?driverId={{driver}}&vehicleNumber={{plate}}
token: {{internal_key}}
HTTP 200
[Asserts]
jsonpath "$.status" == "ACTIVE"
jsonpath "$.route.code" == "{{route_b}}"
jsonpath "$.version" == {{v_changed}}

# Back on route_a, then Start return: the run flips to its direction pair
POST {{base_url}}/internal/sharedCab/route/select
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "agencyId": "{{ibc_id}}", "serviceTierType": "SHARED_CAB", "routeCode": "{{route_a}}", "walkupCount": 0}
HTTP 200
[Asserts]
jsonpath "$.session.route.code" == "{{route_a}}"

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "RETURN"}
HTTP 200
[Asserts]
jsonpath "$.status" == "ACTIVE"
jsonpath "$.route.code" == "{{route_a_return}}"

GET {{base_url}}/internal/sharedCab/trips?driverId={{driver}}&date={{date}}
token: {{internal_key}}
[Options]
retry: 8
retry-interval: 1000ms
HTTP 200
[Asserts]
jsonpath "$.trips[-2:].endReason" nth 0 == "RETURN"
jsonpath "$.trips[-1:].routeCode" == "{{route_a_return}}"

# End at the last stop: session gone, run COMPLETED as END_ROUTE
POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END", "atLastStop": true}
HTTP 200
[Asserts]
body == "null"

GET {{base_url}}/internal/sharedCab/session?driverId={{driver}}&vehicleNumber={{plate}}
token: {{internal_key}}
HTTP 404
[Asserts]
jsonpath "$.errorCode" == "SHARED_CAB_SESSION_NOT_FOUND"

GET {{base_url}}/internal/sharedCab/trips?driverId={{driver}}&date={{date}}
token: {{internal_key}}
[Options]
retry: 8
retry-interval: 1000ms
HTTP 200
[Asserts]
jsonpath "$.trips[-1:].status" == "COMPLETED"
jsonpath "$.trips[-1:].endReason" == "END_ROUTE"
jsonpath "$.trips[-1:].endedAt" exists
