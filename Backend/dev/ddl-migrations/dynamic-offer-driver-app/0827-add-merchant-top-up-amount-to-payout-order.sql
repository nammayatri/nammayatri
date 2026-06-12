ALTER TABLE atlas_driver_offer_bpp.payout_order
  ADD COLUMN IF NOT EXISTS merchant_top_up_amount double precision;
