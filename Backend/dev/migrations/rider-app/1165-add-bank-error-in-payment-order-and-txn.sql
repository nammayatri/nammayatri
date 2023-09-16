ALTER TABLE atlas_app.payment_order ADD COLUMN bank_error_message text;
ALTER TABLE atlas_app.payment_order ADD COLUMN bank_error_code text;
ALTER TABLE atlas_app.payment_transaction ADD COLUMN bank_error_message text;
ALTER TABLE atlas_app.payment_transaction ADD COLUMN bank_error_code text;
