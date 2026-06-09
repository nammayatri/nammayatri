-- Add a GeoJSON-text column for the geometry (city/region boundary) table. The app now
-- reads the polygon from this text column (parsed + point-in-polygon in Haskell, cached
-- in memory) instead of issuing PostGIS ST_Contains / ST_AsGeoJSON at runtime. The legacy
-- `geom` geometry column is left untouched for rollback.
-- Backfill (DML) lives in dev/feature-migrations/0011-backfill-geometry-geom-geojson.sql.
ALTER TABLE atlas_app.geometry ADD COLUMN IF NOT EXISTS geom_geo_json text;
