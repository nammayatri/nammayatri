ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN fake_otp_mobile_numbers text[];

-- DON'T RUN IN MASTER & PROD
ALTER TABLE atlas_driver_offer_bpp.transporter_config
ALTER COLUMN fake_otp_mobile_numbers SET DEFAULT '{}';

-- DON'T RUN IN MASTER & PROD
-- ONLY FOR LOCAL TESTING TO WORK
update atlas_driver_offer_bpp.transporter_config set fake_otp_mobile_numbers = ARRAY ['9999999999'];
