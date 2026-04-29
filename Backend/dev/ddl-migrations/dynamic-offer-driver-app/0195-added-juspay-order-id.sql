ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN payment_service_order_id character varying(255);
ALTER TABLE atlas_driver_offer_bpp.payment_order ALTER COLUMN payment_service_order_id SET NOT NULL;
