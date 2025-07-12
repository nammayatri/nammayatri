ALTER TABLE atlas_bpp_dashboard.merchant
ADD COLUMN enforce_strong_password_policy BOOLEAN DEFAULT FALSE;

ALTER TABLE atlas_bpp_dashboard.merchant
ADD COLUMN password_expiry_days INT;


UPDATE atlas_bpp_dashboard.merchant
SET enforce_strong_password_policy = true,
    password_expiry_days = 45
WHERE short_id = 'MSIL_PARTNER';
