ALTER TABLE atlas_app.payment_order ADD COLUMN payment_merchant_id character varying(255);

UPDATE atlas_app.payment_order SET payment_merchant_id = 'yatrisathi';
