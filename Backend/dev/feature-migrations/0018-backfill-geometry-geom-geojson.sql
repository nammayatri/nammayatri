-- Backfill the geometry table's new geom_geo_json text column from the existing PostGIS
-- geometry column via ST_AsGeoJSON, in both schemas. The column is added by the pure-DDL
-- migrations ddl-migrations/rider-app/1540 and ddl-migrations/dynamic-offer-driver-app/0829.
-- The app now reads city/region boundary polygons from this text column (parsed +
-- point-in-polygon in Haskell, cached in memory) instead of PostGIS at runtime.

UPDATE atlas_app.geometry
SET geom_geo_json = ST_AsGeoJSON(
  CASE
    WHEN ST_SRID(geom) = 4326 THEN geom
    WHEN ST_SRID(geom) = 0 THEN ST_SetSRID(geom, 4326)
    ELSE ST_Transform(geom, 4326)
  END,
  6
)
WHERE geom IS NOT NULL;

UPDATE atlas_driver_offer_bpp.geometry
SET geom_geo_json = ST_AsGeoJSON(
  CASE
    WHEN ST_SRID(geom) = 4326 THEN geom
    WHEN ST_SRID(geom) = 0 THEN ST_SetSRID(geom, 4326)
    ELSE ST_Transform(geom, 4326)
  END,
  6
)
WHERE geom IS NOT NULL;
