-- Enable is2fa_mandatory for BHARAT_TAXI (BAP Delhi) so the 2FA flow works locally.
-- The juspay_admin user has dashboard_access_type = DASHBOARD_ADMIN, but
-- twoFactorMandatoryForRoles only lists DASHBOARD_USER, so is2faRequired is always
-- false for the test user unless is2fa_mandatory is true at the merchant level.

UPDATE atlas_bap_dashboard.merchant
SET is2fa_mandatory = true
WHERE short_id = 'BHARAT_TAXI';

-- Pre-enable 2FA for juspay_admin on BHARAT_TAXI/Delhi with a known test secret
-- so that AirportTaxiFlow's switchMerchantAndCity can get a valid auth token.
-- The collection computes a live TOTP from this secret in its pre-request script.
-- Known test secret (base32): JBSWY3DPEHPK3PXP — NOT used in production.
UPDATE atlas_bap_dashboard.merchant_access
SET secret_key = 'JBSWY3DPEHPK3PXP',
    is2fa_enabled = true
WHERE person_id = '3680f4b5-dce4-4d03-aa8c-5405690e87bd'
  AND merchant_short_id = 'BHARAT_TAXI'
  AND operating_city = 'Delhi';
