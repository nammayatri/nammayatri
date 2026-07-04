-- Fix: geom_geo_json column has web-mercator coordinates (values in millions) for some
-- rows that were inserted before the column existed. Migration 0018 only backfilled
-- rows where geom_geo_json IS NULL, so mercator-encoded rows were left untouched.
-- The Haskell pointInPolygon check uses geom_geo_json, so those rows fail the geometry
-- check with RIDE_NOT_SERVICEABLE even though the PostGIS geom column is correct.
-- Force-overwrite all rows that have a PostGIS geom so they use WGS84 GeoJSON.

UPDATE atlas_app.geometry
SET geom_geo_json = ST_AsGeoJSON(geom)
WHERE geom IS NOT NULL;

UPDATE atlas_driver_offer_bpp.geometry
SET geom_geo_json = ST_AsGeoJSON(geom)
WHERE geom IS NOT NULL;

-- Fix: geometry table has state='Delhi' but Haskell IndianState enum uses
-- 'NationalCapitalTerritory'. Read "Delhi" fails with ColumnTypeMismatch,
-- crashing /v2/rideSearch with INTERNAL_ERROR for ANY city (all geometry
-- rows are fetched to resolve the city polygon).
UPDATE atlas_app.geometry SET state = 'NationalCapitalTerritory' WHERE state = 'Delhi';
UPDATE atlas_driver_offer_bpp.geometry SET state = 'NationalCapitalTerritory' WHERE state = 'Delhi';

-- Fix: Chennai driver_pool_config has single_batch_process_time of 22-25s.
-- SRFD searchRequestValidTill = createdAt + singleBatchProcessTime. The allocator
-- takes ~17s to create SRFD after search, and the test polls ~36s after creation
-- → SRFD already expired (empty searchRequestsForDriver). Bangalore works at 60s;
-- set Chennai to 60s to match.
UPDATE atlas_driver_offer_bpp.driver_pool_config
SET single_batch_process_time = 60
WHERE merchant_operating_city_id IN (
    SELECT moc.id FROM atlas_driver_offer_bpp.merchant_operating_city moc
    WHERE moc.city = 'Chennai'
)
AND single_batch_process_time < 60;
