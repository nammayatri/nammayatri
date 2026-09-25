# 3.6 flush recovery, part 2: run.sh has deleted sharedcab:session:{plate} and sharedcab:route:{route_a}.
# GET session rebuilds the session from the plate's live vehicle_trip row (task 3.5); no re-select needed.
# Untested until a backend is up; needs 3.5 (backend/feat/shared-cab-lts2) merged.

# Restored: same route and run, status from the trip row
GET {{base_url}}/internal/sharedCab/session?driverId={{driver}}&vehicleNumber={{plate}}
token: {{internal_key}}
HTTP 200
[Captures]
v_restored: jsonpath "$.version"
[Asserts]
jsonpath "$.status" == "ACTIVE"
jsonpath "$.route.code" == "{{route_a}}"
jsonpath "$.queuedRoute" == null
jsonpath "$.capacity" == 4
# walkupCount is seeded from the trip's offlineBoardings, which is cumulative for the run: equal here
# (one walk-up, never toggled off), but in general it can exceed the real count, so available errs low.
jsonpath "$.walkupCount" == 1
jsonpath "$.available" == 3
# a rebuilt version comes from the clock, above anything the pre-flush session issued
jsonpath "$.version" > 1000000000

# No new run: the restored session carries on the one that was live
GET {{base_url}}/internal/sharedCab/trips?driverId={{driver}}&date={{date}}
token: {{internal_key}}
HTTP 200
[Asserts]
jsonpath "$.trips[-1:].routeCode" nth 0 == "{{route_a}}"
jsonpath "$.trips[-1:].status" nth 0 == "ACTIVE"
jsonpath "$.trips[-1:].endedAt" nth 0 == null

# The restored session takes writes with its new version
POST {{base_url}}/internal/sharedCab/seats
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "walkupCount": 0, "version": {{v_restored}}}
HTTP 200
[Asserts]
jsonpath "$.status" == "ACTIVE"
jsonpath "$.walkupCount" == 0
jsonpath "$.available" == 4
jsonpath "$.version" > {{v_restored}}

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END"}
HTTP 200
[Asserts]
body == "null"

# Ended for real: the trip row is closed, so nothing is left to restore
GET {{base_url}}/internal/sharedCab/session?driverId={{driver}}&vehicleNumber={{plate}}
token: {{internal_key}}
HTTP 404
[Asserts]
jsonpath "$.errorCode" == "SHARED_CAB_SESSION_NOT_FOUND"

GET {{base_url}}/internal/sharedCab/trips?driverId={{driver}}&date={{date}}
token: {{internal_key}}
HTTP 200
[Asserts]
jsonpath "$.trips[-1:].status" nth 0 == "COMPLETED"
jsonpath "$.trips[-1:].endReason" nth 0 == "END_FOR_NOW"
