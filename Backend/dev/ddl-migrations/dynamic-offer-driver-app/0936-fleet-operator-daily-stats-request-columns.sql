-- Devbox / local stack: columns that the generated migration
-- dev/migrations-read-only/dynamic-offer-driver-app/fleet_operator_daily_stats.sql adds,
-- but that are missing from dev/sql-seed, so a fresh stack's table lacks them.
--
-- Without them the driver drainer fails its first FleetOperatorDailyStats write
-- ("column approved_driver_requests ... does not exist"), stops (force sync is off),
-- and nothing more is copied from Redis to Postgres. The allocator then can't find
-- new search tries (SEARCH_TRY_NOT_FOUND), so 2nd-batch ride requests never go out.
--
-- IF NOT EXISTS keeps this safe on databases that already have the columns.
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN IF NOT EXISTS approved_driver_requests integer;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN IF NOT EXISTS approved_vehicle_requests integer;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN IF NOT EXISTS rejected_driver_requests integer;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN IF NOT EXISTS rejected_vehicle_requests integer;
