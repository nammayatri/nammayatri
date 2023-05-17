ALTER TABLE atlas_app.estimate ADD COLUMN min_customer_extra_fee integer;
ALTER TABLE atlas_app.estimate ADD COLUMN max_customer_extra_fee integer;
UPDATE atlas_app.estimate AS T1 SET min_customer_extra_fee = 0;
UPDATE atlas_app.estimate AS T1 SET max_customer_extra_fee = 100;
ALTER TABLE atlas_app.estimate ALTER COLUMN min_customer_extra_fee SET NOT NULL;
ALTER TABLE atlas_app.estimate ALTER COLUMN max_customer_extra_fee SET NOT NULL;