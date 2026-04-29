-- DON'T RUN IN MASTER & PROD
ALTER TABLE atlas_app.merchant
ALTER COLUMN fake_otp_mobile_numbers SET DEFAULT '{}';
