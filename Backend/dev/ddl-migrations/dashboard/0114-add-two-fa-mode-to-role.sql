-- 2FA refactor: add the two_fa_mode enum column to role. MOBILE_OTP on a role forces the mobile-OTP
-- second factor for every holder; AUTHENTICATOR_APP (default) leaves the per-person mode in effect.
-- Added NULL-able with no default so the one-time backfill in dev/seed-migrations/dashboard can
-- latch on `two_fa_mode IS NULL` and stay rerun-safe. The legacy two_fa_exempt column is retained
-- (no longer read by the app).
ALTER TABLE atlas_dashboard.role
  ADD COLUMN IF NOT EXISTS two_fa_mode text;
