ALTER TABLE fare_parameters ADD COLUMN business_discount DOUBLE PRECISION;
ALTER TABLE fare_policy ADD COLUMN business_discount_percentage DOUBLE PRECISION;
ALTER TABLE fare_parameters ADD COLUMN should_apply_business_discount BOOLEAN default false;