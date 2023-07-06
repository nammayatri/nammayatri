ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN payment_merchant_id character varying(255);

UPDATE atlas_driver_offer_bpp.payment_order SET payment_merchant_id = 'yatrisathi';
