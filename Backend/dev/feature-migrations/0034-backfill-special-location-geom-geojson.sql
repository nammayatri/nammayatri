-- Backfill special_location.geom_geo_json from the existing PostGIS geom column,
-- in both schemas. Without this, getAllEnabledSpecialLocationsWithGeom
-- (Lib/Queries/SpecialLocation.hs) parses an empty polygon list for every
-- special zone (airport/metro/queue), so pointInPolygon never matches and
-- pickups inside a real special zone are treated as area=Default. This broke
-- special-zone OTP generation in NY Rental Airport OTP Flow (BLR Kempegowda
-- Airport pickup returned bookingDetails.contents.otpCode = null) and would
-- affect any other special-zone flow (metro pickup, queue-based pickup, other
-- airports) the same way — every row in both schemas had geom_geo_json NULL.
--
-- This is the special_location counterpart of 0018's geometry table backfill
-- (different table: city/region boundaries vs. named pickup zones).

UPDATE atlas_app.special_location
SET geom_geo_json = ST_AsGeoJSON(geom)
WHERE geom IS NOT NULL
  AND geom_geo_json IS NULL;

UPDATE atlas_driver_offer_bpp.special_location
SET geom_geo_json = ST_AsGeoJSON(geom)
WHERE geom IS NOT NULL
  AND geom_geo_json IS NULL;
