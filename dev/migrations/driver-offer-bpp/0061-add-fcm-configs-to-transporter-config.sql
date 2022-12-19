ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN fcm_url text;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN fcm_service_account text;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN fcm_token_key_prefix text;

-- local testing data
INSERT INTO atlas_driver_offer_bpp.transporter_config (merchant_id, pickup_loc_threshold, drop_loc_threshold, fcm_url, fcm_service_account, fcm_token_key_prefix) VALUES
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f','500','500','https://fcm.googleapis.com/v1/projects/jp-beckn-dev/messages:send/','<base-64-version-of-fcm-json>', 'ardu-bpp');

ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN fcm_service_account SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN fcm_url SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN fcm_token_key_prefix SET NOT NULL;

