# Rider OTP login; prints the verify response, whose .token the rider flow needs.
# /v2/auth is rate limited (HITS_LIMIT_EXCEED): log in once and reuse the token.

POST {{base_url}}/v2/auth
Content-Type: application/json
{
  "mobileNumber": "{{mobileNumber}}",
  "mobileCountryCode": "+91",
  "merchantId": "{{merchantId}}",
  "deviceToken": "{{deviceToken}}",
  "registrationLat": {{from_lat}},
  "registrationLon": {{from_lon}},
  "merchantOperatingCityId": "{{merchantOperatingCityId}}"
}
HTTP 200
[Captures]
authId: jsonpath "$.authId"

POST {{base_url}}/v2/auth/{{authId}}/verify
Content-Type: application/json
{
  "otp": "{{otp}}",
  "deviceToken": "{{deviceToken}}",
  "whatsappNotificationEnroll": "OPT_IN"
}
HTTP 200
[Asserts]
jsonpath "$.token" exists
