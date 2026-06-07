-- Backfill the new geom_geo_json text columns from the existing PostGIS geometry
-- columns via ST_AsGeoJSON, for gate_info and special_location in both schemas.
-- The columns themselves are added by the pure-DDL migrations:
--   ddl-migrations/rider-app/1538, 1539
--   ddl-migrations/dynamic-offer-driver-app/0827, 0828
-- The application now reads zone/pickup polygons from these text columns (parsed +
-- point-in-polygon in Haskell, cached in memory) instead of PostGIS at runtime.

UPDATE atlas_app.gate_info
SET geom_geo_json = ST_AsGeoJSON(geom)
WHERE geom IS NOT NULL
  AND geom_geo_json IS NULL;

UPDATE atlas_app.special_location
SET geom_geo_json = ST_AsGeoJSON(geom)
WHERE geom IS NOT NULL
  AND geom_geo_json IS NULL;

UPDATE atlas_driver_offer_bpp.gate_info
SET geom_geo_json = ST_AsGeoJSON(geom)
WHERE geom IS NOT NULL
  AND geom_geo_json IS NULL;

UPDATE atlas_driver_offer_bpp.special_location
SET geom_geo_json = ST_AsGeoJSON(geom)
WHERE geom IS NOT NULL
  AND geom_geo_json IS NULL;
