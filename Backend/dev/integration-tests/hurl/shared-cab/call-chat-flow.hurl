# Masked call and chat: the leg status carries the ExoPhone and the chat channel while a cab holds the seat; the driver session lists
# the channel per rider; rider-app hands out the rider's number only to the booking's driver; the ExoPhone dial-in resolves the
# driver. All of it closes when the rider gets down.
# Needs --variable token=<rider token>, rider_config.shared_cab_call_enabled and shared_cab_chat_enabled on for the city, a real
# driver (the dial-in asks driver-app for the number) and the variables exophone, driver_base_url and driver_token (that driver's
# driver-app token) in the env file; the driver call needs the Exotel service reachable (a local mock answers it).
# No LTS pings here, so the proximity anchor is the board stop (as in boarding-flow.hurl).

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END", "force": true}
HTTP *

POST {{base_url}}/internal/sharedCab/route/select
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "agencyId": "{{ibc_id}}", "serviceTierType": "SHARED_CAB", "capacity": 4, "routeCode": "{{route_a}}", "walkupCount": 0, "driverName": "{{driver_name}}", "vehicleModel": "{{vehicle_model}}"}
HTTP 200
[Asserts]
jsonpath "$.session.status" == "ACTIVE"

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

# ---- 1. allocated: the rider's side ----
GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
[Options]
retry: 10
retry-interval: 3000ms
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" matches "^(ALLOCATED|ARRIVING)$"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.maskedCallNumber" matches "^0?{{exophone}}$"
[Captures]
bookingId: jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.bookingId"

# same window, same answer: five more polls of the same leg must not blink
GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.maskedCallNumber" matches "^0?{{exophone}}$"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.chatChannelId" == "{{bookingId}}"

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.maskedCallNumber" matches "^0?{{exophone}}$"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.chatChannelId" == "{{bookingId}}"

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.maskedCallNumber" matches "^0?{{exophone}}$"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.chatChannelId" == "{{bookingId}}"

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.maskedCallNumber" matches "^0?{{exophone}}$"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.chatChannelId" == "{{bookingId}}"

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.maskedCallNumber" matches "^0?{{exophone}}$"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.chatChannelId" == "{{bookingId}}"

# the chat channel is the booking id, and no driver number rides on the leg status
GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.chatChannelId" == "{{bookingId}}"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.driverMobileNumber" not exists

# ---- 2. the driver's side ----
GET {{base_url}}/internal/sharedCab/session
token: {{internal_key}}
[QueryStringParams]
driverId: {{driver}}
vehicleNumber: {{plate}}
HTTP 200
[Asserts]
jsonpath "$.ridersByStop[*].boarding[?(@.bookingId == '{{bookingId}}')].chatChannelId" == "{{bookingId}}"
jsonpath "$.ridersByStop[*].boarding[?(@.bookingId == '{{bookingId}}')].callable" == true

# ---- 3. rider-app hands the rider's number to this booking's driver only ----
GET {{base_url}}/internal/sharedCab/booking/{{bookingId}}/callNumber
token: {{internal_key}}
[QueryStringParams]
driverId: {{driver}}
vehicleNumber: {{plate}}
HTTP 200
[Asserts]
jsonpath "$.riderNumber" == "+91{{mobileNumber}}"

# another driver on this booking's plate, and the right driver naming another plate: each is refused on its own
GET {{base_url}}/internal/sharedCab/booking/{{bookingId}}/callNumber
token: {{internal_key}}
[QueryStringParams]
driverId: {{driver_b}}
vehicleNumber: {{plate}}
HTTP 400
[Asserts]
jsonpath "$.errorCode" == "SHARED_CAB_CALL_NOT_AVAILABLE"

GET {{base_url}}/internal/sharedCab/booking/{{bookingId}}/callNumber
token: {{internal_key}}
[QueryStringParams]
driverId: {{driver}}
vehicleNumber: {{plate_b}}
HTTP 400
[Asserts]
jsonpath "$.errorCode" == "SHARED_CAB_CALL_NOT_AVAILABLE"

GET {{base_url}}/internal/sharedCab/booking/{{bookingId}}/callNumber
token: wrong-key
[QueryStringParams]
driverId: {{driver}}
vehicleNumber: {{plate}}
HTTP *
[Asserts]
jsonpath "$.errorCode" exists
jsonpath "$.riderNumber" not exists

# ---- 3b. the driver calls the rider through driver-app (Exotel connects driver and rider) ----
POST {{driver_base_url}}/ui/sharedCab/booking/{{bookingId}}/call
token: {{driver_token}}
HTTP 200
[Captures]
callId: jsonpath "$.callId"
[Asserts]
jsonpath "$.callId" exists

GET {{driver_base_url}}/ui/sharedCab/call/{{callId}}/status
token: {{driver_token}}
HTTP 200
[Asserts]
jsonpath "$.callStatusId" == "{{callId}}"
jsonpath "$.entityId" == "{{bookingId}}"

# Exotel reports the connect failed: the callback must record it, not look the booking id up as a ride
POST {{driver_base_url}}/ui/driver/ride/call/statusCallback
Content-Type: application/json
{"CallSid": "mock-failed", "EventType": "terminal", "DateCreated": "2026-01-01 00:00:00", "DateUpdated": "2026-01-01 00:00:00", "From": "1", "To": "2", "Status": "failed", "PhoneNumberSid": "x", "StartTime": "2026-01-01 00:00:00", "EndTime": "2026-01-01 00:00:00", "Direction": "outbound-api", "RecordingUrl": "", "ConversationDuration": 0, "Legs": [], "CustomField": "{\"callStatusId\": \"{{callId}}\", \"entityId\": \"{{bookingId}}\"}"}
HTTP 200

GET {{driver_base_url}}/ui/sharedCab/call/{{callId}}/status
token: {{driver_token}}
HTTP 200
[Asserts]
jsonpath "$.status" == "FAILED"

# ---- 3c. the driver messages the rider: the push goes out only for this booking's driver ----
POST {{driver_base_url}}/ui/sharedCab/booking/{{bookingId}}/message
token: {{driver_token}}
Content-Type: application/json
{"message": "on my way"}
HTTP 200

POST {{driver_base_url}}/ui/sharedCab/booking/{{bookingId}}/message
token: {{driver_token}}
Content-Type: application/json
{"message": "   "}
HTTP 400

POST {{base_url}}/internal/sharedCab/booking/{{bookingId}}/message
token: {{internal_key}}
{"driverId": "{{driver_b}}", "vehicleNumber": "{{plate}}", "message": "x"}
HTTP 400
[Asserts]
jsonpath "$.errorCode" == "SHARED_CAB_CHAT_NOT_AVAILABLE"

POST {{base_url}}/internal/sharedCab/booking/{{bookingId}}/message
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate_b}}", "message": "x"}
HTTP 400
[Asserts]
jsonpath "$.errorCode" == "SHARED_CAB_CHAT_NOT_AVAILABLE"

# ---- 4. the rider dials the ExoPhone: Exotel's passthru asks rider-app for the driver's number ----
GET {{base_url}}/v2/exotel/call/driver/number
[QueryStringParams]
CallSid: hurl-call-{{bookingId}}
CallFrom: 0{{mobileNumber}}
CallTo: {{exophone}}
CallStatus: ringing
To: {{exophone}}
HTTP 200
[Asserts]
jsonpath "$" matches "^\\+[0-9]{10,14}$"

# Exotel may report the dialled number with its leading zero while the stored row has it or not: both spellings reach the driver
GET {{base_url}}/v2/exotel/call/driver/number
[QueryStringParams]
CallSid: hurl-call-zero-{{bookingId}}
CallFrom: 0{{mobileNumber}}
CallTo: 0{{exophone}}
CallStatus: ringing
To: 0{{exophone}}
HTTP 200
[Asserts]
jsonpath "$" matches "^\\+[0-9]{10,14}$"

# ---- 5. boarded: still inside the window ----
# the rider stands where the cab is (board_lat / board_lon), so the code is accepted without a confirmation
POST {{base_url}}/v2/multimodal/{{journeyId}}/rider/location
token: {{token}}
Content-Type: application/json
{"latLong": {"lat": {{board_lat}}, "lon": {{board_lon}}}, "currTime": "{{newDate}}"}
HTTP 200

POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setOnboardedVehicleDetails
token: {{token}}
Content-Type: application/json
{"vehicleNumber": "{{plate_code}}", "supportsBoardingConfirmation": true}
HTTP 200
[Asserts]
jsonpath "$.boardingConfirmationRequired" == null

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "BOARDED"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.maskedCallNumber" matches "^0?{{exophone}}$"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.chatChannelId" == "{{bookingId}}"

# ---- 6. got down: everything closes ----
POST {{base_url}}/v2/multimodal/{{journeyId}}/order/{{legOrder}}/subleg/1/setStatus/Completed
token: {{token}}
HTTP 200
[Asserts]
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.state" == "DROPPED"
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.maskedCallNumber" == null
jsonpath "$.legs[?(@.legOrder == {{legOrder}})].sharedCab.chatChannelId" == null

GET {{base_url}}/internal/sharedCab/booking/{{bookingId}}/callNumber
token: {{internal_key}}
[QueryStringParams]
driverId: {{driver}}
vehicleNumber: {{plate}}
HTTP 400
[Asserts]
jsonpath "$.errorCode" == "SHARED_CAB_CALL_NOT_AVAILABLE"

POST {{driver_base_url}}/ui/sharedCab/booking/{{bookingId}}/call
token: {{driver_token}}
HTTP 400
[Asserts]
jsonpath "$.errorCode" == "SHARED_CAB_CALL_NOT_AVAILABLE"

POST {{driver_base_url}}/ui/sharedCab/booking/{{bookingId}}/message
token: {{driver_token}}
Content-Type: application/json
{"message": "too late"}
HTTP 400
[Asserts]
jsonpath "$.errorCode" == "SHARED_CAB_CHAT_NOT_AVAILABLE"

GET {{base_url}}/v2/exotel/call/driver/number
[QueryStringParams]
CallSid: hurl-call-after-{{bookingId}}
CallFrom: 0{{mobileNumber}}
CallTo: {{exophone}}
CallStatus: ringing
To: {{exophone}}
HTTP *
[Asserts]
jsonpath "$.errorCode" exists

POST {{base_url}}/internal/sharedCab/route/end
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "next": "END", "force": true}
HTTP *
