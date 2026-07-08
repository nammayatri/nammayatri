-- Backfill the geometry table's new geom_geo_json text column from the existing PostGIS
-- geometry column via ST_AsGeoJSON, in both schemas. The column is added by the pure-DDL
-- migrations ddl-migrations/rider-app/1540 and ddl-migrations/dynamic-offer-driver-app/0829.
-- The app now reads city/region boundary polygons from this text column (parsed +
-- point-in-polygon in Haskell, cached in memory) instead of PostGIS at runtime.

-- No "AND geom_geo_json IS NULL" guard: geom is the source of truth, so any
-- row whose geom_geo_json has gone stale (doesn't match geom) must be
-- resynced too, not just rows where it was never set.

UPDATE atlas_app.geometry
SET geom_geo_json = ST_AsGeoJSON(geom)
WHERE geom IS NOT NULL;

UPDATE atlas_driver_offer_bpp.geometry
SET geom_geo_json = ST_AsGeoJSON(geom)
WHERE geom IS NOT NULL;

-- Backfill geom_geo_json for special_location tables too.
-- The special zone detection code (Lib/Queries/SpecialLocation.hs) uses
-- geom_geo_json for point-in-polygon checks. Without this, special zones
-- like T1: IGI Airport are not detected → wrong fare products used.

UPDATE atlas_app.special_location
SET geom_geo_json = ST_AsGeoJSON(geom)
WHERE geom IS NOT NULL;

UPDATE atlas_driver_offer_bpp.special_location
SET geom_geo_json = ST_AsGeoJSON(geom)
WHERE geom IS NOT NULL;
