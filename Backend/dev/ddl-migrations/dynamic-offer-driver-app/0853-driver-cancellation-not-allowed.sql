-- NOTE: dont need to run these queries
alter table atlas_driver_offer_bpp.fare_policy add column IF NOT EXISTS driver_cancellation_not_allowed boolean;
alter table atlas_driver_offer_bpp.fare_parameters add column IF NOT EXISTS driver_cancellation_not_allowed boolean;
-- NOTE: dont need to run these queries
