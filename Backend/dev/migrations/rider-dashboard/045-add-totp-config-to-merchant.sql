ALTER TABLE atlas_bap_dashboard.merchant ADD COLUMN totp_step_size int;
ALTER TABLE atlas_bap_dashboard.merchant ADD COLUMN totp_clock_skew int;

-- Backfill sensible defaults (30s step, TwoSteps = ±60s skew) matching cryptonite's defaultTOTPParams.
UPDATE atlas_bap_dashboard.merchant SET totp_step_size = 30 WHERE totp_step_size IS NULL;
UPDATE atlas_bap_dashboard.merchant SET totp_clock_skew = 2 WHERE totp_clock_skew IS NULL;
