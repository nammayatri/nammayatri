ALTER TABLE atlas_app.merchant ADD COLUMN fake_otp_mobile_numbers text[];
ALTER TABLE atlas_app.person ADD COLUMN use_fake_otp text;

-- DON'T RUN IN MASTER & PROD
ALTER TABLE atlas_app.merchant
ALTER COLUMN fake_otp_mobile_numbers SET DEFAULT '{}';

-- DON'T RUN IN MASTER & PROD
-- ONLY FOR LOCAL TESTING TO WORK
update atlas_app.merchant set fake_otp_mobile_numbers = ARRAY ['9999999999'];