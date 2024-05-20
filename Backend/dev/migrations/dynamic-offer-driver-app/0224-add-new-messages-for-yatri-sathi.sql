-- LOCAL SYNC
UPDATE atlas_driver_offer_bpp.transporter_config SET enable_dashboard_sms = true;
  --WHERE merchant_id = 'favorit0-0000-0000-0000-00000favorit'; -- change merchant id to merchant id of yatri sathi
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN enable_dashboard_sms SET NOT NULL;

INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message) VALUES
    ('favorit0-0000-0000-0000-00000favorit', 'END_RIDE_MESSAGE',
'Dear User,

Ride completed successfully
Ride Amount : {#rideAmount#}
Trip ID : {#rideId#}

- Juspay'); -- change merchant id to merchant id of yatri sathi

INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message) VALUES
    ('favorit0-0000-0000-0000-00000favorit', 'ONBOARDING_YATRI_MESSAGE',
'Dear User,

You are successfully onboarded to Yatri Sathi

- Juspay'); -- change merchant id to merchant id of yatri sathi

INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message) VALUES
    ('favorit0-0000-0000-0000-00000favorit', 'BOOKING_MESSAGE',
'Dear User,

Please find your trip details below:

Ride OTP : {#otp#}
Ride Amount : {#amount#}

- Juspay'); -- change merchant id to merchant id of yatri sathi

INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message) VALUES
    ('favorit0-0000-0000-0000-00000favorit', 'CASH_COLLECTED_MESSAGE',
'Dear User,

Payment of {#amount#} received in cash towards booth charges.

- Juspay'); -- change merchant id to merchant id of yatri sathi