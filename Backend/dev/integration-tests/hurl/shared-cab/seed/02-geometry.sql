-- The apps read geom_geo_json (in-memory cached); the LTS needs Backend/geo_config/<name>.json separately (see README).
INSERT INTO atlas_app.geometry (id, region, city, state, geom)
SELECT md5('e2e-shillong-geom-app')::uuid::text, 'Chennai', 'Chennai', 'TamilNadu', ST_SetSRID(ST_MakeEnvelope(91.80,25.49,92.02,25.69),0)
WHERE NOT EXISTS (SELECT 1 FROM atlas_app.geometry WHERE id=md5('e2e-shillong-geom-app')::uuid::text);
INSERT INTO atlas_driver_offer_bpp.geometry (id, region, city, state, geom)
SELECT md5('e2e-shillong-geom-drv')::uuid::text, 'Chennai', 'Chennai', 'TamilNadu', ST_SetSRID(ST_MakeEnvelope(91.80,25.49,92.02,25.69),0)
WHERE NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.geometry WHERE id=md5('e2e-shillong-geom-drv')::uuid::text);
UPDATE atlas_app.geometry SET geom_geo_json='{"type":"MultiPolygon","coordinates":[[[[91.80,25.49],[92.02,25.49],[92.02,25.69],[91.80,25.69],[91.80,25.49]]]]}' WHERE id=md5('e2e-shillong-geom-app')::uuid::text; -- apps use geom_geo_json (in-mem cached), not geom
