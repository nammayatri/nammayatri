-- 2FA refactor: add the two_fa_mode enum column (AUTHENTICATOR_APP | MOBILE_OTP) to person.
-- AUTHENTICATOR_APP = TOTP authenticator (enrolment tracked by secret_key); MOBILE_OTP = an OTP
-- texted to the registered mobile number. Added NULL-able with no default so the one-time backfill
-- in dev/seed-migrations/dashboard can latch on `two_fa_mode IS NULL` and stay rerun-safe (DML is
-- not allowed under dev/ddl-migrations). The legacy two_fa_exempt / is2fa_enabled columns are
-- intentionally retained (no longer read by the app).
ALTER TABLE atlas_dashboard.person
  ADD COLUMN IF NOT EXISTS two_fa_mode text;
