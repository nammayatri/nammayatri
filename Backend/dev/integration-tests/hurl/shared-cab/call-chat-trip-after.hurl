# The booking now names a trip that is not the cab's running one: same plate, same driver, boarded on another trip. Call and chat are
# closed everywhere. Needs --variable token, journeyId, legOrder, bookingId from call-chat-trip-before.hurl.

GET {{base_url}}/v2/multimodal/journey/{{journeyId}}/status
token: {{token}}
HTTP 200
[Asserts]
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

POST {{base_url}}/internal/sharedCab/booking/{{bookingId}}/message
token: {{internal_key}}
{"driverId": "{{driver}}", "vehicleNumber": "{{plate}}", "message": "hello"}
HTTP 400
[Asserts]
jsonpath "$.errorCode" == "SHARED_CAB_CHAT_NOT_AVAILABLE"

GET {{base_url}}/internal/sharedCab/session
token: {{internal_key}}
[QueryStringParams]
driverId: {{driver}}
vehicleNumber: {{plate}}
HTTP 200
[Asserts]
jsonpath "$.ridersByStop[*].alighting[?(@.bookingId == '{{bookingId}}')].callable" == false
jsonpath "$.ridersByStop[*].alighting[?(@.bookingId == '{{bookingId}}')].chatChannelId" == null

GET {{base_url}}/v2/exotel/call/driver/number
[QueryStringParams]
CallSid: hurl-call-trip-{{bookingId}}
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
