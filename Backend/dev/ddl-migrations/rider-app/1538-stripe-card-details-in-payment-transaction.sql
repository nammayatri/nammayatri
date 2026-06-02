ALTER TABLE atlas_app.payment_transaction ADD COLUMN card_exp_month integer;
ALTER TABLE atlas_app.payment_transaction ADD COLUMN card_exp_year integer;
ALTER TABLE atlas_app.payment_transaction ADD COLUMN card_country text;
ALTER TABLE atlas_app.payment_transaction ADD COLUMN card_fingerprint text;
