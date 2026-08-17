-- Partial index for the driver-supply metrics publisher: per-city count of active drivers,
-- executed on the read replica every 60s per operating city.
-- CONCURRENTLY: driver_information is one of the hottest write tables (every online/offline
-- toggle); a plain build would block all writes for the duration. The runner applies these
-- files via plain psql (autocommit), so CONCURRENTLY is valid here — house style per
-- migrations 0822/0830/0839/0847. If a concurrent build fails it leaves an INVALID index:
-- drop and re-run.
create index concurrently if not exists idx_driver_information_city_active on atlas_driver_offer_bpp.driver_information (merchant_operating_city_id) where active = true;
