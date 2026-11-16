-- Book for others: SMS sent to the passenger when a driver is assigned to a ride someone
-- else booked for them. Variables: {#bookedByName#} {#passengerName#} {#driverName#}
-- {#vehicleNumber#} {#otp#} {#rideLink#}.
--
-- Seeded for every city that already has FOLLOW_RIDE configured, reusing its SMS sender header.
-- NOTE: template_id must be replaced with the DLT-approved template id per merchant before prod.
INSERT INTO atlas_app.merchant_message (
    merchant_id,
    merchant_operating_city_id,
    message_key,
    message,
    template_id,
    sender_header,
    contains_url_button,
    created_at,
    updated_at
)
SELECT
    mm.merchant_id,
    mm.merchant_operating_city_id,
    'BOOKED_FOR_OTHER_RIDE_DETAILS',
    'Hi {#passengerName#}, {#bookedByName#} has booked a ride for you. Driver {#driverName#} ({#vehicleNumber#}) is on the way. Ride OTP: {#otp#}. Track live: {#rideLink#} -Namma Yatri',
    NULL,
    mm.sender_header,
    false,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_message mm
WHERE mm.message_key = 'FOLLOW_RIDE'
ON CONFLICT (merchant_operating_city_id, message_key) DO NOTHING;
