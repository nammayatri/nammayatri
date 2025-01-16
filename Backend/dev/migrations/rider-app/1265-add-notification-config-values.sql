INSERT INTO atlas_app.ride_related_notification_config (
    id, merchant_id, merchant_operating_city_id, time_diff, time_diff_event, notification_type, notification_key, on_booking_status, on_scheduled_booking, event_time
    )
VALUES
(atlas_app.uuid_generate_v4(), '4b17bd06-ae7e-48e9-85bf-282fb310209c','b30daaf7-77d2-17c8-00d9-baf7ad0f5719', 2700, 'PICKUP_TIME', 'SMS', 'SCHEDULED_RIDE_REMINDER' , 'TRIP_ASSIGNED', True, 'PreEvent'),
(atlas_app.uuid_generate_v4(), '4b17bd06-ae7e-48e9-85bf-282fb310209c','b30daaf7-77d2-17c8-00d9-baf7ad0f5719', 5400, 'PICKUP_TIME', 'PN', 'RIDE_START_REMINDER' , 'TRIP_ASSIGNED', True, 'PreEvent');



INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
VALUES ('4b17bd06-ae7e-48e9-85bf-282fb310209c', 'SCHEDULED_RIDE_REMINDER', 'Your ride start OTP is {#rideStartOtp#} and ride end OTP is {#rideEndOtp#}. Driver will reach 15 minutes prior to scheduled start time.', 'b30daaf7-77d2-17c8-00d9-baf7ad0f5719');

-- DO NOT RUN TOLL_CROSSED QUERY IN PROD
INSERT INTO atlas_app.merchant_push_notification (key, merchant_id, merchant_operating_city_id, title, body, language, fcm_notification_type)
VALUES
('RIDE_START_REMINDER', '4b17bd06-ae7e-48e9-85bf-282fb310209c', 'b30daaf7-77d2-17c8-00d9-baf7ad0f5719', '{#isRentalOrIntercity#} Booking Reminder.', 'Ride starts at {#rideStartTime#}. Details will be shared shortly. ', 'ENGLISH', 'SCHEDULED_RIDE_NOTIFICATION'),
('TOLL_CROSSED', '4b17bd06-ae7e-48e9-85bf-282fb310209c', 'b30daaf7-77d2-17c8-00d9-baf7ad0f5719', 'Toll Crossed !', 'Toll charges added to final fare', 'ENGLISH', 'TOLL_CROSSED');