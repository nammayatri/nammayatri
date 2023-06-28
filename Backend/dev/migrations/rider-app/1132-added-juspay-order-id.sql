ALTER TABLE atlas_app.payment_order ADD COLUMN payment_service_order_id character varying(255);
UPDATE atlas_app.payment_order SET payment_service_order_id = 'UNKNOWN';
ALTER TABLE atlas_app.payment_order ALTER COLUMN payment_service_order_id SET NOT NULL;
