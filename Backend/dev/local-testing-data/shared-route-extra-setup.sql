-- ****************** --
-- NOT FOR PRODUCTION --
-- ****************** --


-- Shared-route (corporate shuttle) extra setup for LOCAL testing: the config every shuttle module integrates against, on top of
-- the seeded dev stack. LOCAL ONLY. The corporate feed is hosted on the imported Chennai NAMMA_YATRI city, so the ids below are the
-- local seed's (rider city c7e3c3eb-…, driver city f8e9db0a-…). Run by hand, after the stack is up:
--   psql -h localhost -p 5434 -U atlas_superuser -d atlas_dev -f Backend/dev/local-testing-data/shared-route-extra-setup.sql
-- then clear the apps' Redis config caches and restart the processes. Idempotent.
--
-- Schema is not here: the columns come from the storage YAML (migrations-read-only): ride.trip_id / route_code /
-- integrated_bpp_config_id, frfs_ticket_booking.ride_id, transporter_config.shared_run_config.
--
-- The fixture is in three pieces; this file is the config piece, pure SQL so the dev stack's context API can apply it with the
-- other local-testing-data files. The other two live in dev/scripts/shared-route-fixture/ (README there):
--   1. GTFS feed `corp_chennai`: corp_chennai.gtfs/, loaded into the local GIMS by hand. Agency `corp_chennai`; route CORP-ACME-1
--      with stops CORP_ACME_S1 … CORP_ACME_S5; trips CORP-ACME-1-0830 (S1 to S5) and CORP-ACME-1-1800 (S5 to S1), Monday to
--      Friday, stop times 10 minutes apart. Module C's publish tool replaces the hand-made folder later with the same ids.
--   2. People and vehicle: apply-fixture-data.sh (needs the dev passetto for the encrypted columns). One fleet owner (the NY-owned
--      entity, "ACME shuttle fleet"), two drivers under it (the second tagged DCO), one BUS_NON_AC bus bound to route CORP-ACME-1
--      (vehicle_route_mapping), three riders. Phones 99999001xx, OTP 7891.
--
-- Convention this fixture follows (Plan tab, "GTFS convention"): feed key corp_<city>, agency key = the integrated bpp
-- config's agency key, route code CORP-<org>-<n>, trip id <route>-<HHMM>, calendar = service days.

-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
-- 1. Feed config: integrated_bpp_config rows for the corporate feed (rider MULTIMODAL + APPLICATION, driver APPLICATION),
--    with the demand knobs on the rider rows' DIRECT provider config.
-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
-- Module C: the city's OTP and GIMS endpoints point at the local servers that serve the corp_chennai feed (OTP on 8089, GIMS on
-- 8090), as shared-cab-extra-setup.sql does for its feed. The feed itself is published from the control centre.
UPDATE atlas_app.merchant_service_config SET config_json = jsonb_set(config_json::jsonb,'{baseUrl}','"http://localhost:8089/otp/gtfs/v1/"')::json
 WHERE service_name='MultiModal_OTPTransit' AND merchant_operating_city_id='c7e3c3eb-cc15-46d4-ba04-5af55ac87874';
UPDATE atlas_app.merchant_service_config SET config_json = jsonb_set(config_json::jsonb,'{baseUrl}','"http://localhost:8090"')::json
 WHERE service_name='MultiModalStaticData_OTPTransit' AND merchant_operating_city_id='c7e3c3eb-cc15-46d4-ba04-5af55ac87874';

-- rider MULTIMODAL row, cloned from the imported Chennai bus DIRECT row so the OTP/GTFS endpoints and cipher key carry over
INSERT INTO atlas_app.integrated_bpp_config (id, agency_key, domain, feed_key, merchant_id, merchant_operating_city_id, platform_type, config_json, vehicle_category, created_at, updated_at)
SELECT md5('e2e-corp-shuttle-ibc')::uuid::text, 'corp_chennai', 'FRFS', 'corp_chennai', '4b17bd06-ae7e-48e9-85bf-282fb310209c', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', 'MULTIMODAL', config_json, 'BUS', now(), now()
FROM atlas_app.integrated_bpp_config WHERE agency_key = 'chennai_data_bus' AND merchant_operating_city_id = 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874' LIMIT 1
ON CONFLICT DO NOTHING;

-- rider APPLICATION row (routeServiceability and the driver-side lookups read this platform type)
INSERT INTO atlas_app.integrated_bpp_config (id, agency_key, domain, feed_key, merchant_id, merchant_operating_city_id, platform_type, config_json, vehicle_category, created_at, updated_at)
SELECT md5('e2e-corp-shuttle-ibc-app')::uuid::text, agency_key, domain, feed_key, merchant_id, merchant_operating_city_id, 'APPLICATION', config_json, vehicle_category, now(), now()
FROM atlas_app.integrated_bpp_config WHERE id = md5('e2e-corp-shuttle-ibc')::uuid::text
ON CONFLICT DO NOTHING;

-- Demand knobs: the corporate shuttle preset. Fixed route, timetabled runs, numbered seats, booked before departure, the run
-- pre-assigned on the driver side, boarding by per-booking OTP, cancel free until 30 minutes before, a late cancel or a
-- no-show costs one trip of the subscription. Every value is from Domain.Types.SharedRoute.
UPDATE atlas_app.integrated_bpp_config
SET config_json = jsonb_set(config_json::jsonb, '{contents,sharedKnobs}',
  '{"route":"FIXED","schedule":"TIMETABLED","seats":"RESERVED","booking":"PRE_BOOK","allocation":"PRE_ASSIGNED","boarding":"OTP","cancelCutoffMinutes":30,"noShowConsequence":"FORFEIT_TRIP"}')::json
WHERE id IN (md5('e2e-corp-shuttle-ibc')::uuid::text, md5('e2e-corp-shuttle-ibc-app')::uuid::text);

-- driver side: same id and agency key (they must match across the two DBs), BUS category
INSERT INTO atlas_driver_offer_bpp.integrated_bpp_config (id, agency_key, domain, feed_key, merchant_id, merchant_operating_city_id, platform_type, config_json, vehicle_category, city, created_at, updated_at)
SELECT md5('e2e-corp-shuttle-ibc')::uuid::text, 'corp_chennai', 'FRFS', 'corp_chennai', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'f8e9db0a-96c8-49e4-942a-3e3f7265d2da', 'APPLICATION', config_json, 'BUS', 'Chennai', now(), now()
FROM atlas_driver_offer_bpp.integrated_bpp_config WHERE agency_key = 'chennai_bus' LIMIT 1
ON CONFLICT DO NOTHING;

-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
-- 2. Supply knobs: the driver city's transporter_config.shared_run_config. The materialiser reads the timetable of every
--    feed in `feeds` (here the corporate feed only). PER_TICKET fee (commission from the SharedRoute fare policy on the sum
--    of ticket fares), 30 min between a driver's runs, driver asked to check in 90 min before departure, fallback job takes
--    an unacknowledged run at 45 min before, 2 min wait at a stop before a no-show.
-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
UPDATE atlas_driver_offer_bpp.transporter_config
SET shared_run_config = '{"schedule":"TIMETABLED","feeds":["corp_chennai"],"driverFee":"PER_TICKET","clashWindowMinutes":30,"fallbackWindowMinutes":45,"checkInOffsetMinutes":90,"noShowWaitSeconds":120}'
WHERE merchant_operating_city_id = 'f8e9db0a-96c8-49e4-942a-3e3f7265d2da';

-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
-- 3. Ticket fares (rider side): the SHUTTLE service tier on the corporate feed, one MatrixBased fare policy, the route fare
--    product for CORP-ACME-1 and a flat 40 INR for every ordered stop pair of its five stops (S1 to S5 for the morning trip,
--    S5 to S1 for the evening return).
-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
INSERT INTO atlas_app.frfs_vehicle_service_tier (type,description,id,integrated_bpp_config_id,long_name,merchant_id,merchant_operating_city_id,provider_code,short_name,created_at,updated_at,is_air_conditioned,pay_on_board)
VALUES ('SHUTTLE','Corporate shuttle',md5('e2e-corp-tier')::uuid::text,md5('e2e-corp-shuttle-ibc')::uuid::text,'Corporate Shuttle','4b17bd06-ae7e-48e9-85bf-282fb310209c','c7e3c3eb-cc15-46d4-ba04-5af55ac87874','SHUTTLE','Shuttle',now(),now(),true,false) ON CONFLICT DO NOTHING;
DELETE FROM atlas_app.route_stop_fare WHERE fare_policy_id = md5('e2e-corp-fp-CORP-ACME-1')::uuid::text;
INSERT INTO atlas_app.frfs_fare_policy (id,type,description,merchant_id,merchant_operating_city_id,created_at,updated_at)
VALUES (md5('e2e-corp-fp-CORP-ACME-1')::uuid::text,'MatrixBased','corporate shuttle seat fare CORP-ACME-1','4b17bd06-ae7e-48e9-85bf-282fb310209c','c7e3c3eb-cc15-46d4-ba04-5af55ac87874',now(),now()) ON CONFLICT DO NOTHING;
INSERT INTO atlas_app.frfs_route_fare_product (fare_policy_id,id,integrated_bpp_config_id,merchant_id,merchant_operating_city_id,route_code,time_bounds,vehicle_service_tier_id,vehicle_type,created_at,updated_at)
VALUES (md5('e2e-corp-fp-CORP-ACME-1')::uuid::text,md5('e2e-corp-rfp-CORP-ACME-1')::uuid::text,md5('e2e-corp-shuttle-ibc')::uuid::text,'4b17bd06-ae7e-48e9-85bf-282fb310209c','c7e3c3eb-cc15-46d4-ba04-5af55ac87874','CORP-ACME-1','Unbounded',md5('e2e-corp-tier')::uuid::text,'BUS',now(),now()) ON CONFLICT DO NOTHING;
-- every ordered pair (i <> j) of S1..S5 at 40 INR: i < j serves the 08:30 trip, i > j the 18:00 return
INSERT INTO atlas_app.route_stop_fare (amount,currency,end_stop_code,fare_policy_id,integrated_bpp_config_id,merchant_id,merchant_operating_city_id,start_stop_code,category,created_at,updated_at)
SELECT 40,'INR','CORP_ACME_S'||j,md5('e2e-corp-fp-CORP-ACME-1')::uuid::text,md5('e2e-corp-shuttle-ibc')::uuid::text,'4b17bd06-ae7e-48e9-85bf-282fb310209c','c7e3c3eb-cc15-46d4-ba04-5af55ac87874','CORP_ACME_S'||i,'ADULT',now(),now()
FROM generate_series(1,5) AS i, generate_series(1,5) AS j WHERE i <> j;

-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
-- 4. Run fare product (driver side): the SharedRoute row the materialiser prices a run from, cloned from the city's on-demand
--    row so it points at an existing fare policy. The run's fare is the sum of its ticket fares at run end; this policy's
--    commission_charge_config is what PER_TICKET applies to it (set it on the policy when the fee is decided).
-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
INSERT INTO atlas_driver_offer_bpp.fare_product (id, area, enabled, fare_policy_id, merchant_id, merchant_operating_city_id, time_bounds, trip_category, vehicle_variant, search_source)
SELECT md5('e2e-corp-run-fare-product')::uuid::text, area, true, fare_policy_id, merchant_id, merchant_operating_city_id, 'Unbounded', 'SharedRoute_OnDemandStaticOffer', 'BUS_NON_AC', search_source
FROM atlas_driver_offer_bpp.fare_product
WHERE merchant_operating_city_id = 'f8e9db0a-96c8-49e4-942a-3e3f7265d2da' AND trip_category = 'OneWay_OneWayOnDemandDynamicOffer' AND enabled
ORDER BY id LIMIT 1
ON CONFLICT DO NOTHING;

-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
-- 5. The company (rider side): one pass organization for ACME. pass_enum reuses RegularPass until module C decides whether a
--    corporate pass type is needed; the admin person is the first seeded rider of the city. Email domains: set below (module C).
-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
INSERT INTO atlas_app.pass_organization (id, name, pass_enum, person_id, merchant_id, merchant_operating_city_id, address, created_at, updated_at)
SELECT md5('e2e-corp-org-acme')::uuid::text, 'ACME Corp', 'RegularPass', p.id, '4b17bd06-ae7e-48e9-85bf-282fb310209c', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874', 'ACME campus, OMR, Chennai', now(), now()
FROM atlas_app.person p WHERE p.merchant_operating_city_id = 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874' ORDER BY p.created_at LIMIT 1
ON CONFLICT DO NOTHING;

-- Module C: ACME's work-email domain. A rider whose verified business email is @acme.example belongs to ACME
-- (SharedLogic.CorporateMembership); the dashboard sets this through passOrganization/update's emailDomains.
UPDATE atlas_app.pass_organization SET email_domains = '{acme.example}' WHERE id = md5('e2e-corp-org-acme')::uuid::text;
