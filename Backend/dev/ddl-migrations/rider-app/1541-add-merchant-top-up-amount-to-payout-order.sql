ALTER TABLE atlas_app.payout_order
  ADD COLUMN IF NOT EXISTS merchant_top_up_amount double precision;
