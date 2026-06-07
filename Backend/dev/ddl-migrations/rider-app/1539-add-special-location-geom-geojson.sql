-- Add a GeoJSON-text column for special-location zone polygons. The application reads
-- the zone polygon from this text column (parsed + point-in-polygon done in Haskell,
-- cached in memory) instead of issuing PostGIS ST_Contains / ST_DWithin / ST_AsGeoJSON
-- at runtime. The legacy `geom` geometry column is left untouched for rollback.
-- Backfill (DML) lives in dev/feature-migrations/0010-backfill-geom-geojson.sql.
ALTER TABLE atlas_app.special_location ADD COLUMN IF NOT EXISTS geom_geo_json text;
