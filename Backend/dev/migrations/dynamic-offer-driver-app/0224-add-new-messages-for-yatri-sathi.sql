ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN enable_dashboard_sms Boolean;
UPDATE atlas_driver_offer_bpp.transporter_config SET enable_dashboard_sms=false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN enable_dashboard_sms SET NOT NULL;

UPDATE atlas_driver_offer_bpp.transporter_config SET enable_dashboard_sms = true
  WHERE merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f'; -- change merchant id to merchant id of yatri sathi

INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message) VALUES
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'END_RIDE_MESSAGE',
'Dear User,

Ride completed successfully
Ride Amount : {#rideAmount#}
Trip ID : {#rideId#}

- Juspay'); -- change merchant id to merchant id of yatri sathi

INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message) VALUES
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'ONBOARDING_YATRI_MESSAGE',
'Dear User,

You are successfully onboarded to Yatri Sathi

- Juspay'); -- change merchant id to merchant id of yatri sathi

INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message) VALUES
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'BOOKING_MESSAGE',
'Dear User,

Please find your trip details below:

Ride OTP : {#otp#}
Ride Amount : {#amount#}

- Juspay'); -- change merchant id to merchant id of yatri sathi

INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message) VALUES
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'CASH_COLLECTED_MESSAGE',
'Dear User,

Payment of {#amount#} received in cash towards booth charges.

- Juspay'); -- change merchant id to merchant id of yatri sathi