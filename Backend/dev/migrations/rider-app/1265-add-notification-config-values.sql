INSERT INTO atlas_app.ride_related_notification_config (
    id, merchant_id, merchant_operating_city_id, time_diff, time_diff_event, notification_type, notification_key, on_booking_status, on_scheduled_booking, event_time
    )
VALUES
(atlas_app.uuid_generate_v4(), '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f','1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', 2700, 'PICKUP_TIME', 'SMS', 'SCHEDULED_RIDE_REMINDER' , 'TRIP_ASSIGNED', True, 'PreEvent'),
(atlas_app.uuid_generate_v4(), '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f','1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', 5400, 'PICKUP_TIME', 'PN', 'RIDE_START_REMINDER' , 'TRIP_ASSIGNED', True, 'PreEvent');



INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message)
VALUES ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'SCHEDULED_RIDE_REMINDER', 'Your ride start OTP is {#rideStartOtp#} and ride end OTP is {#rideEndOtp#}. Driver will reach 15 minutes prior to scheduled start time.');


INSERT INTO atlas_app.merchant_push_notifications (key, merchantId, merchantOperatingCityId, title, body, language, fcmNotificationType)
VALUES
( 'RIDE_START_REMINDER', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', '{#IsRentalOrIntercity#} Booking Reminder.', 'Ride starts at {#rideStartTime#}. Details will be shared shortly. ', 'ENGLISH', 'SCHEDULED_RIDE_NOTIFICATION'),
( 'TOLL_CROSSED', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', 'Toll Crossed !', 'Toll charges added to final fare', 'ENGLISH', 'TOLL_CROSSED');