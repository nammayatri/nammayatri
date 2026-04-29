ALTER TABLE atlas_driver_offer_bpp.payment_transaction ALTER COLUMN txn_uuid DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payment_transaction ALTER COLUMN payment_method_type DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payment_transaction ALTER COLUMN payment_method DROP NOT NULL;