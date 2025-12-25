ALTER TABLE atlas_app.payment_order ADD COLUMN payment_fulfillment_status text;
ALTER TABLE atlas_app.payment_order ADD COLUMN domain_entity_id text;
ALTER TABLE atlas_app.payment_order ADD COLUMN domain_transaction_id text;