-- Fix: special_location and gate_info tables are missing the geom_geo_json column
-- which the Haskell code (SpecialLocation.hs, GateInfo.hs) expects.
-- Without this column, all queries that touch special_location fail with:
--   "column special_location.geom_geo_json does not exist"
-- This crashes the mapConcurrently in getAllFarePoliciesProduct, causing
-- farePoliciesCount=0 and empty estimates for ALL search requests.
--
-- Issue: gate_info uses "point" column not "geom" (a gate is a single GPS
-- coordinate, not a polygon), so the UPDATE must be guarded with a
-- column-existence check to avoid crashing the entire migration and
-- preventing downstream fixes (OSRM, batch timing) from being applied.

-- atlas_driver_offer_bpp schema
ALTER TABLE atlas_driver_offer_bpp.special_location
  ADD COLUMN IF NOT EXISTS geom_geo_json TEXT;

UPDATE atlas_driver_offer_bpp.special_location
  SET geom_geo_json = ST_AsGeoJSON(geom)
  WHERE geom IS NOT NULL AND geom_geo_json IS NULL;

ALTER TABLE atlas_driver_offer_bpp.gate_info
  ADD COLUMN IF NOT EXISTS geom_geo_json TEXT;

-- gate_info uses "point" not "geom"; only backfill if geom column exists
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM information_schema.columns
             WHERE table_schema = 'atlas_driver_offer_bpp'
               AND table_name = 'gate_info'
               AND column_name = 'geom') THEN
    EXECUTE 'UPDATE atlas_driver_offer_bpp.gate_info
             SET geom_geo_json = ST_AsGeoJSON(geom)
             WHERE geom IS NOT NULL AND geom_geo_json IS NULL';
  END IF;
END $$;

-- atlas_app schema
ALTER TABLE atlas_app.special_location
  ADD COLUMN IF NOT EXISTS geom_geo_json TEXT;

UPDATE atlas_app.special_location
  SET geom_geo_json = ST_AsGeoJSON(geom)
  WHERE geom IS NOT NULL AND geom_geo_json IS NULL;

ALTER TABLE atlas_app.gate_info
  ADD COLUMN IF NOT EXISTS geom_geo_json TEXT;

-- gate_info uses "point" not "geom"; only backfill if geom column exists
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM information_schema.columns
             WHERE table_schema = 'atlas_app'
               AND table_name = 'gate_info'
               AND column_name = 'geom') THEN
    EXECUTE 'UPDATE atlas_app.gate_info
             SET geom_geo_json = ST_AsGeoJSON(geom)
             WHERE geom IS NOT NULL AND geom_geo_json IS NULL';
  END IF;
END $$;

-- Fix: updated_at column is "timestamp without time zone" but Haskell Beam
-- expects "timestamp with time zone" (UTCTime). Without this fix,
-- GateInfoExtra.hs:47 throws ColumnTypeMismatch reading gate_info rows,
-- killing the mapConcurrently and causing 0 fare policies.

ALTER TABLE atlas_driver_offer_bpp.special_location
  ALTER COLUMN updated_at TYPE timestamp with time zone USING updated_at AT TIME ZONE 'UTC';
ALTER TABLE atlas_driver_offer_bpp.gate_info
  ALTER COLUMN updated_at TYPE timestamp with time zone USING updated_at AT TIME ZONE 'UTC';
ALTER TABLE atlas_app.special_location
  ALTER COLUMN updated_at TYPE timestamp with time zone USING updated_at AT TIME ZONE 'UTC';
ALTER TABLE atlas_app.gate_info
  ALTER COLUMN updated_at TYPE timestamp with time zone USING updated_at AT TIME ZONE 'UTC';

-- Fix: estimate_breakup_list_json is stored as text but Beam reads it as
-- Aeson.Value (backed by ByteString). Beam can't deserialize text -> ByteString,
-- causing ColumnTypeMismatch in getQuotes (500 on GET /rideSearch/:id/results).
ALTER TABLE atlas_app.estimate
  ALTER COLUMN estimate_breakup_list_json TYPE jsonb USING estimate_breakup_list_json::jsonb;

-- Fix: single_batch_process_time controls both (a) how long a driver has to
-- respond to a search request (searchRequestValidTill = now + batchTime), and
-- (b) how long before the allocator dispatches the next batch. Default 60s is
-- too long for integration tests (drivers can't see the request before it's
-- polled). 30s gives enough time for test polling + response while staying fast.
UPDATE atlas_driver_offer_bpp.driver_pool_config
SET single_batch_process_time = 30
WHERE merchant_operating_city_id IN (
    SELECT moc.id FROM atlas_driver_offer_bpp.merchant_operating_city moc
    WHERE moc.city IN ('Bangalore', 'Delhi', 'Chennai', 'Kolkata')
)
AND single_batch_process_time > 30;

-- Fix: getEstimatedPickupDistances defaults to Google, but mock-google doesn't
-- support distance matrix. OSRM is available locally with Bangalore map data.
-- Without this, computeActualDistance returns errors/huge values, filtering out
-- all drivers (totalCandidates=N offRideFinal=0) during select processing.
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET get_estimated_pickup_distances = 'OSRM'
WHERE merchant_operating_city_id IN (
    SELECT moc.id FROM atlas_driver_offer_bpp.merchant_operating_city moc
    WHERE moc.city IN ('Bangalore', 'Delhi', 'Chennai', 'Kolkata')
);

-- Fix: finance_account table is missing the sub_ledger column which the Haskell
-- Beam schema (Account.hs) expects. Without it, on_confirm crashes with
-- "column t0.sub_ledger does not exist" in the Account query, leaving bookings
-- stuck at status=NEW with no OTP -> Start Ride fails with INCORRECT_OTP.
ALTER TABLE atlas_app.finance_account ADD COLUMN IF NOT EXISTS sub_ledger TEXT;
ALTER TABLE atlas_driver_offer_bpp.finance_account ADD COLUMN IF NOT EXISTS sub_ledger TEXT;