-- Mirror of rider-app migration 1561. payment_transaction is defined once in lib/payment
-- and used by both apps, so the beam table must line up in both schemas even though only
-- rider-app's FRFSCCAvenueSplitPayout cron writes these columns today.
ALTER TABLE atlas_driver_offer_bpp.payment_transaction ADD COLUMN split_payout_response json;
ALTER TABLE atlas_driver_offer_bpp.payment_transaction ADD COLUMN split_payout_success boolean;
