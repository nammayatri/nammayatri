-- Payout destination shown to the driver: the institution name and the masked tail of the
-- account, read from the Stripe connected account. Never the full account number.
-- No backfill: rows populate on the driver's next bankAccount/status call.
ALTER TABLE atlas_driver_offer_bpp.driver_bank_account ADD COLUMN IF NOT EXISTS bank_name text;
ALTER TABLE atlas_driver_offer_bpp.driver_bank_account ADD COLUMN IF NOT EXISTS bank_account_last4 text;
