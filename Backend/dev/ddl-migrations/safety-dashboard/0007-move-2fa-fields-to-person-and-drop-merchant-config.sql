-- 2FA refactor: enforcement is now deployment-level (env config) and enrolment
-- lives on the person, not per-merchant.

ALTER TABLE atlas_safety_dashboard.person
  ADD COLUMN IF NOT EXISTS secret_key text,
  ADD COLUMN IF NOT EXISTS is2fa_enabled boolean NOT NULL DEFAULT false;

ALTER TABLE atlas_safety_dashboard.merchant_access
  DROP COLUMN IF EXISTS secret_key,
  DROP COLUMN IF EXISTS is2fa_enabled;

ALTER TABLE atlas_safety_dashboard.merchant
  DROP COLUMN IF EXISTS is2fa_mandatory,
  DROP COLUMN IF EXISTS enforcement_deadline,
  DROP COLUMN IF EXISTS two_fa_otp_ttl_in_secs,
  DROP COLUMN IF EXISTS two_fa_max_otp_verify_attempts,
  DROP COLUMN IF EXISTS totp_step_size,
  DROP COLUMN IF EXISTS totp_clock_skew,
  DROP COLUMN IF EXISTS email_otp_ttl_in_secs,
  DROP COLUMN IF EXISTS email_max_otp_verify_attempts;
