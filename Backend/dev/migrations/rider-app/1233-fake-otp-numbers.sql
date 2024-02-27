ALTER TABLE atlas_app.merchant ADD COLUMN fake_otp_mobile_numbers text[];
ALTER TABLE atlas_app.person ADD COLUMN use_fake_otp text;

-- DON'T RUN IN MASTER & PROD
ALTER TABLE atlas_app.merchant
ALTER COLUMN fake_otp_mobile_numbers SET DEFAULT '{}';
