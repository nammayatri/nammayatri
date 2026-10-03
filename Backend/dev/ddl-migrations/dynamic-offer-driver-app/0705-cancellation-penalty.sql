-- NOTE: dont need to run these queries
alter table atlas_driver_offer_bpp.fare_parameters add column IF NOT EXISTS driver_cancellation_penalty_amount double precision;
alter table atlas_driver_offer_bpp.fare_policy add column IF NOT EXISTS driver_cancellation_penalty_amount double precision;
-- NOTE: dont need to run these queries
