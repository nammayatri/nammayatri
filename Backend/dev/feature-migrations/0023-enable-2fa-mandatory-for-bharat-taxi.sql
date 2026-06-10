-- Enable is2fa_mandatory for BHARAT_TAXI (BAP Delhi) so the 2FA flow works locally.
-- The juspay_admin user has dashboard_access_type = DASHBOARD_ADMIN, but
-- twoFactorMandatoryForRoles only lists DASHBOARD_USER, so is2faRequired is always
-- false for the test user unless is2fa_mandatory is true at the merchant level.

UPDATE atlas_bap_dashboard.merchant
SET is2fa_mandatory = true
WHERE short_id = 'BHARAT_TAXI';
