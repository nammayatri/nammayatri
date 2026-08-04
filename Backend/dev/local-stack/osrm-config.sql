-- Point both services at OSRM for anything that needs the road network.
--
-- Routing is the one thing mock-google cannot do. It implements DistanceMatrix,
-- PlaceName and SnapToRoad, but there is no Directions endpoint at all, so
-- get_routes = Google returns 404 and a ride search dies before it reaches
-- BECKN. OSRM is free, self-hosted and unlimited, and the backend already
-- supports it -- so this is configuration, not code.
--
-- Split of responsibilities after this runs:
--
--   get_distances  OSRM     road distances, not straight lines
--   get_routes     OSRM     the actual reason for all of this
--   snap_to_road   OSRM     GPS traces onto real roads
--   get_place_name Google   -> mock-google; OSRM is a router, not a geocoder
--   auto_complete  Google   -> mock-google, same reason
--
-- Address search and autocomplete are the remaining gap: mock-google returns
-- fixture data for those. A real deployment needs a geocoder (Nominatim
-- self-hosted, or a paid provider). It does not block ride search.
--
-- Idempotent: safe to re-run.

BEGIN;

-- Rider (BAP). Maps_OSRM already exists here -- migration
-- 1057-merchant-config-changes.sql inserts it for every merchant.
UPDATE atlas_app.merchant_service_usage_config
   SET get_distances = 'OSRM',
       get_routes    = 'OSRM',
       snap_to_road  = 'OSRM';

-- Driver (BPP). Its migrations insert Maps_OSRM too, so in a normal run this
-- INSERT does nothing. It is kept as a guard: switching the usage config to a
-- provider with no matching config row would leave maps silently broken on the
-- driver side, and that is a bad thing to discover from a failing ride search.
INSERT INTO atlas_driver_offer_bpp.merchant_service_config
       (merchant_id, service_name, config_json)
SELECT m.id,
       'Maps_OSRM',
       json_build_object('osrmUrl', 'localhost:5000', 'radiusDeviation', 20)
  FROM atlas_driver_offer_bpp.merchant m
 WHERE NOT EXISTS (
         SELECT 1
           FROM atlas_driver_offer_bpp.merchant_service_config c
          WHERE c.merchant_id = m.id
            AND c.service_name = 'Maps_OSRM');

UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
   SET get_distances = 'OSRM',
       get_routes    = 'OSRM',
       snap_to_road  = 'OSRM';

COMMIT;
