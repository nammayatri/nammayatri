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
--   snap_to_road   OSRM     GPS traces onto real roads
--   get_routes     Google   -> maps-shim -> OSRM   (see below)
--   get_place_name Google   -> maps-shim -> mock-google
--   auto_complete  Google   -> maps-shim -> mock-google
--
-- get_routes stays on "Google" deliberately. OSRM cannot serve it in this
-- baseline -- Kernel.External.Maps.Interface.OSRM exports only callOsrmMatch,
-- getDistances and getOSRMTable, so selecting OSRM fails with
--     "Function getRoutes is not provided by service OSRM"
-- Instead the Google endpoint is repointed at maps-shim, which speaks the
-- Directions API and answers from OSRM. The backend cannot tell, and the
-- routes come from the real Algerian road graph rather than a fixture.
--
-- Address search and autocomplete still return mock-google's fixture data,
-- forwarded through the shim. A real deployment needs a geocoder (Nominatim
-- self-hosted, or a paid provider). Neither blocks ride search.
--
-- Idempotent: safe to re-run.

BEGIN;

-- Rider (BAP). Maps_OSRM already exists here -- migration
-- 1057-merchant-config-changes.sql inserts it for every merchant.
UPDATE atlas_app.merchant_service_usage_config
   SET get_distances = 'OSRM',
       get_routes    = 'Google',   -- served by maps-shim, backed by OSRM
       snap_to_road  = 'OSRM';

-- Repoint "Google" at the shim rather than at mock-google directly. The shim
-- forwards everything except /directions/json straight through, so place
-- names and autocomplete are unaffected. googleKey is preserved: it is an
-- encrypted value and the shim ignores it, but the backend still decrypts it.
UPDATE atlas_app.merchant_service_config
   SET config_json = json_build_object(
         'googleMapsUrl',  'http://localhost:8030/',
         'googleRoadsUrl', 'http://localhost:8030/',
         'googleKey',      config_json ->> 'googleKey')
 WHERE service_name = 'Maps_Google';

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
       get_routes    = 'Google',   -- served by maps-shim, backed by OSRM
       snap_to_road  = 'OSRM';

UPDATE atlas_driver_offer_bpp.merchant_service_config
   SET config_json = json_build_object(
         'googleMapsUrl',  'http://localhost:8030/',
         'googleRoadsUrl', 'http://localhost:8030/',
         'googleKey',      config_json ->> 'googleKey')
 WHERE service_name = 'Maps_Google';

COMMIT;
