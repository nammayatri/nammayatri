-- Point-in-time snapshot of the payout destination, so history stays accurate if the
-- driver later changes bank. Not backfilled: rows before this deploy keep NULLs.
ALTER TABLE atlas_app.payout_request ADD COLUMN IF NOT EXISTS bank_name text;
ALTER TABLE atlas_app.payout_request ADD COLUMN IF NOT EXISTS bank_account_last4 text;
