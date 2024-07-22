ALTER TABLE atlas_app.payment_transaction ADD COLUMN application_fee_amount double precision;
ALTER TABLE atlas_app.payment_transaction ADD COLUMN retry_count integer;