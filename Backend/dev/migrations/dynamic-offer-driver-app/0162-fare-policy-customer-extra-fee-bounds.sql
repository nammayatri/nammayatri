CREATE TABLE atlas_driver_offer_bpp.fare_policy_customer_extra_fee_bounds (
  id serial PRIMARY KEY,
  fare_policy_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.fare_policy(id),
  start_distance integer NOT NULL,
  min_fee integer NOT NULL,
  max_fee integer NOT NULL,
  CONSTRAINT fare_policy_customer_extra_fee_bounds_unique_start_distance UNIQUE (fare_policy_id, start_distance)
);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_customer_extra_fee_bounds OWNER TO atlas_driver_offer_bpp_user;

WITH ExtraFeeBounds1 AS (
  SELECT T1.id,
    0,
    0,
    30
  FROM atlas_driver_offer_bpp.fare_policy AS T1
)
INSERT INTO atlas_driver_offer_bpp.fare_policy_customer_extra_fee_bounds (
  fare_policy_id,
  start_distance,
  min_fee,
  max_fee)
  (SELECT * FROM ExtraFeeBounds1);

ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN min_customer_extra_fee integer;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN max_customer_extra_fee integer;
UPDATE atlas_driver_offer_bpp.estimate AS T1 SET min_customer_extra_fee = 0;
UPDATE atlas_driver_offer_bpp.estimate AS T1 SET max_customer_extra_fee = 100;
ALTER TABLE atlas_driver_offer_bpp.estimate ALTER COLUMN min_customer_extra_fee SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.estimate ALTER COLUMN max_customer_extra_fee SET NOT NULL;