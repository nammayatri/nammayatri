-- Mirror of rider-app/1559: lib/payment is shared, so the driver schema needs the same column
-- for the Beam table to match. Not populated on this side today (driver payments use Juspay).
ALTER TABLE atlas_driver_offer_bpp.payment_transaction ADD COLUMN payment_method_id text;
