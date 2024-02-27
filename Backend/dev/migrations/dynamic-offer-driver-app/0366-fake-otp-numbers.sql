ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN fake_otp_mobile_numbers text[];
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN use_fake_otp text;

-- DON'T RUN IN MASTER & PROD
ALTER TABLE atlas_driver_offer_bpp.transporter_config
ALTER COLUMN fake_otp_mobile_numbers SET DEFAULT '{}';
