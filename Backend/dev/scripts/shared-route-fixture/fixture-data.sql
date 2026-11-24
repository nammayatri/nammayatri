-- ****************** --
-- NOT FOR PRODUCTION --
-- ****************** --

-- Shared-route (corporate shuttle) dev fixture, DATA half: the people and the vehicle every shuttle module tests against, on top of
-- the config half (dev/local-testing-data/shared-route-extra-setup.sql) and the seeded dev stack. LOCAL ONLY.
--
-- Do not run this file directly, and do not move it into dev/local-testing-data (the context API applies every *.sql there verbatim):
-- the *_encrypted columns need passetto blobs and the *_hash columns the dev salt, so run it through
--   Backend/dev/scripts/shared-route-fixture/apply-fixture-data.sh
-- which fetches them from the local passetto (started by `, run-mobility-stack-dev`) and passes them in as psql variables.
-- Idempotent: every row is keyed on a fixed md5 id and guarded, re-running changes nothing.
--
-- What it creates (ids are md5('e2e-corp-…')::uuid, phones are 99999001xx, dev OTP 7891, login tokens 'e2e-corp-<who>-token'):
--   driver side (merchant 7f7896dd…, Chennai city f8e9db0a…)
--     fleet owner  e2e-corp-fleet-owner   9999900110  "ACME shuttle fleet": the Namma Yatri-owned entity
--     driver 1     e2e-corp-driver-1      9999900111  owned driver, drives the bus
--     driver 2     e2e-corp-driver-2      9999900112  curated (DCO) driver, in the same fleet, tag DCO
--     bus          TN01SH0001             BUS_NON_AC, 20 seats, RC on driver 1, bound to route CORP-ACME-1 (vehicle_route_mapping)
--   rider side (merchant 4b17bd06…, Chennai city c7e3c3eb…)
--     riders       e2e-corp-rider-1/2/3   9999900121/122/123   Asha, Bala, Charu; work-email verification lands with module C
--
-- Variables expected (set by the .sh): fo_enc fo_hash d1_enc d1_hash d2_enc d2_hash bus_enc bus_hash r1_enc r1_hash r2_enc r2_hash
-- r3_enc r3_hash. *_hash is hex without the \x prefix.

-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
-- 1. Fleet owner (driver side): person, login token, fleet_owner_information
-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
INSERT INTO atlas_driver_offer_bpp.person
  (id, first_name, last_name, gender, identifier_type, is_new, merchant_id, merchant_operating_city_id, onboarded_from_dashboard, role,
   total_earned_coins, used_coins, mobile_country_code, mobile_number_encrypted, mobile_number_hash, unencrypted_mobile_number, created_at, updated_at)
SELECT md5('e2e-corp-fleet-owner')::uuid::text, 'ACME', 'Fleet', 'UNKNOWN', 'MOBILENUMBER', false,
       '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'f8e9db0a-96c8-49e4-942a-3e3f7265d2da', true, 'FLEET_OWNER',
       0, 0, '+91', :'fo_enc', decode(:'fo_hash', 'hex'), '9999900110', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.person WHERE id = md5('e2e-corp-fleet-owner')::uuid::text);

INSERT INTO atlas_driver_offer_bpp.registration_token
  (id, attempts, auth_expiry, auth_medium, auth_type, auth_value_hash, entity_id, entity_type, merchant_id, token, token_expiry, verified, created_at, updated_at)
SELECT md5('e2e-corp-fleet-owner-token')::uuid::text, 0, 365, 'SMS', 'OTP', '7891', md5('e2e-corp-fleet-owner')::uuid::text,
       'USER                              ', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'e2e-corp-fleet-owner-token', 365, true, now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.registration_token WHERE id = md5('e2e-corp-fleet-owner-token')::uuid::text);

INSERT INTO atlas_driver_offer_bpp.fleet_owner_information
  (fleet_owner_person_id, merchant_id, merchant_operating_city_id, fleet_type, enabled, verified, blocked, is_eligible_for_subscription, fleet_name, created_at, updated_at)
SELECT md5('e2e-corp-fleet-owner')::uuid::text, '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'f8e9db0a-96c8-49e4-942a-3e3f7265d2da',
       'NORMAL_FLEET', true, true, false, true, 'ACME shuttle fleet', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.fleet_owner_information WHERE fleet_owner_person_id = md5('e2e-corp-fleet-owner')::uuid::text);

-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
-- 2. Two drivers: person, login token, driver_information, driver_stats, fleet association. Driver 2 carries the DCO tag
--    (tagName#tagValue, as Yudhishthira stores it) so the scheduled-ride board can show curated runs to curated drivers only.
-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
INSERT INTO atlas_driver_offer_bpp.person
  (id, first_name, last_name, gender, identifier_type, is_new, merchant_id, merchant_operating_city_id, onboarded_from_dashboard, role,
   total_earned_coins, used_coins, mobile_country_code, mobile_number_encrypted, mobile_number_hash, unencrypted_mobile_number, created_at, updated_at)
SELECT md5('e2e-corp-driver-1')::uuid::text, 'Dinesh', 'Owned', 'UNKNOWN', 'MOBILENUMBER', false,
       '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'f8e9db0a-96c8-49e4-942a-3e3f7265d2da', false, 'DRIVER',
       0, 0, '+91', :'d1_enc', decode(:'d1_hash', 'hex'), '9999900111', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.person WHERE id = md5('e2e-corp-driver-1')::uuid::text);

INSERT INTO atlas_driver_offer_bpp.person
  (id, first_name, last_name, gender, identifier_type, is_new, merchant_id, merchant_operating_city_id, onboarded_from_dashboard, role,
   total_earned_coins, used_coins, mobile_country_code, mobile_number_encrypted, mobile_number_hash, unencrypted_mobile_number, driver_tag, created_at, updated_at)
SELECT md5('e2e-corp-driver-2')::uuid::text, 'Dev', 'Curated', 'UNKNOWN', 'MOBILENUMBER', false,
       '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'f8e9db0a-96c8-49e4-942a-3e3f7265d2da', false, 'DRIVER',
       0, 0, '+91', :'d2_enc', decode(:'d2_hash', 'hex'), '9999900112', ARRAY['DCO#true'], now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.person WHERE id = md5('e2e-corp-driver-2')::uuid::text);
-- a driver 2 seeded before the tag existed gets it
UPDATE atlas_driver_offer_bpp.person SET driver_tag = ARRAY['DCO#true'], updated_at = now()
WHERE id = md5('e2e-corp-driver-2')::uuid::text AND NOT (COALESCE(driver_tag, ARRAY[]::text[]) @> ARRAY['DCO#true']);

INSERT INTO atlas_driver_offer_bpp.registration_token
  (id, attempts, auth_expiry, auth_medium, auth_type, auth_value_hash, entity_id, entity_type, merchant_id, token, token_expiry, verified, created_at, updated_at)
SELECT md5('e2e-corp-driver-' || n || '-token')::uuid::text, 0, 365, 'SMS', 'OTP', '7891', md5('e2e-corp-driver-' || n)::uuid::text,
       'USER                              ', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'e2e-corp-driver-' || n || '-token', 365, true, now(), now()
FROM generate_series(1, 2) AS n
WHERE NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.registration_token WHERE id = md5('e2e-corp-driver-' || n || '-token')::uuid::text);

INSERT INTO atlas_driver_offer_bpp.driver_information
  (driver_id, merchant_id, merchant_operating_city_id, enabled, verified, active, subscribed, created_at, updated_at)
SELECT md5('e2e-corp-driver-' || n)::uuid::text, '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'f8e9db0a-96c8-49e4-942a-3e3f7265d2da', true, true, true, true, now(), now()
FROM generate_series(1, 2) AS n
WHERE NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.driver_information WHERE driver_id = md5('e2e-corp-driver-' || n)::uuid::text);

INSERT INTO atlas_driver_offer_bpp.driver_stats (driver_id, idle_since, updated_at)
SELECT md5('e2e-corp-driver-' || n)::uuid::text, now(), now()
FROM generate_series(1, 2) AS n
WHERE NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.driver_stats WHERE driver_id = md5('e2e-corp-driver-' || n)::uuid::text);

INSERT INTO atlas_driver_offer_bpp.fleet_driver_association
  (id, driver_id, fleet_owner_id, is_active, associated_on, associated_till, created_at, updated_at)
SELECT md5('e2e-corp-fda-' || n)::uuid::text, md5('e2e-corp-driver-' || n)::uuid::text, md5('e2e-corp-fleet-owner')::uuid::text,
       true, now(), '2099-12-31T00:00:00Z', now(), now()
FROM generate_series(1, 2) AS n
WHERE NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.fleet_driver_association WHERE id = md5('e2e-corp-fda-' || n)::uuid::text);

-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
-- 3. The bus: vehicle on driver 1, its RC (active), and the route binding the materialiser reads to pre-assign the run
--    (vehicle_route_mapping, keyed on route code + vehicle number hash, as the fleet "add vehicle to route" API writes it).
-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
INSERT INTO atlas_driver_offer_bpp.vehicle
  (driver_id, merchant_id, merchant_operating_city_id, color, model, make, registration_no, variant, vehicle_class, category, capacity, selected_service_tiers, created_at, updated_at)
SELECT md5('e2e-corp-driver-1')::uuid::text, '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'f8e9db0a-96c8-49e4-942a-3e3f7265d2da',
       'White', 'Starbus', 'Tata', 'TN01SH0001', 'BUS_NON_AC', 'M3', 'BUS', 20, ARRAY['BUS_NON_AC'], now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.vehicle WHERE driver_id = md5('e2e-corp-driver-1')::uuid::text);

INSERT INTO atlas_driver_offer_bpp.vehicle_registration_certificate
  (id, document_image_id, fitness_expiry, verification_status, certificate_number_hash, certificate_number_encrypted, failed_rules,
   vehicle_class, vehicle_manufacturer, vehicle_model, vehicle_variant, vehicle_color, vehicle_capacity, merchant_id, created_at, updated_at)
SELECT md5('e2e-corp-bus-rc')::uuid::text, md5('e2e-corp-bus-rc-img')::uuid::text, now() + interval '365 days', 'VALID',
       decode(:'bus_hash', 'hex'), :'bus_enc', ARRAY[]::text[],
       'M3', 'Tata', 'Starbus', 'BUS_NON_AC', 'White', 20, '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.vehicle_registration_certificate WHERE id = md5('e2e-corp-bus-rc')::uuid::text);

INSERT INTO atlas_driver_offer_bpp.driver_rc_association
  (id, driver_id, rc_id, associated_on, associated_till, consent, consent_timestamp, is_rc_active, created_at, updated_at)
SELECT md5('e2e-corp-bus-rc-assoc')::uuid::text, md5('e2e-corp-driver-1')::uuid::text, md5('e2e-corp-bus-rc')::uuid::text,
       now(), '2099-12-31T00:00:00Z', true, now(), true, now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.driver_rc_association WHERE id = md5('e2e-corp-bus-rc-assoc')::uuid::text);

INSERT INTO atlas_driver_offer_bpp.vehicle_route_mapping
  (blocked, fleet_owner_id, merchant_id, merchant_operating_city_id, route_code, vehicle_number_encrypted, vehicle_number_hash, created_at, updated_at)
SELECT false, md5('e2e-corp-fleet-owner')::uuid::text, '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'f8e9db0a-96c8-49e4-942a-3e3f7265d2da',
       'CORP-ACME-1', :'bus_enc', decode(:'bus_hash', 'hex'), now(), now()
ON CONFLICT (route_code, vehicle_number_hash) DO NOTHING;

-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
-- 4. Three riders (rider side): person and login token each. Riders 1 and 2 are ACME staff (email domain acme.example once
--    module C's work-email verification lands); rider 3 is the walk-up who should be refused a subscription.
-- ════════════════════════════════════════════════════════════════════════════════════════════════════════════
INSERT INTO atlas_app.person
  (id, blocked, gender, has_taken_valid_ride, identifier_type, is_new, is_valid_rating, role, merchant_id, merchant_operating_city_id,
   mobile_country_code, mobile_number_encrypted, mobile_number_hash, unencrypted_mobile_number, first_name, last_name, created_at, updated_at)
SELECT md5('e2e-corp-rider-1')::uuid::text, false, 'UNKNOWN', false, 'MOBILENUMBER', false, true, 'USER',
       '4b17bd06-ae7e-48e9-85bf-282fb310209c', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874',
       '+91', :'r1_enc', decode(:'r1_hash', 'hex'), '9999900121', 'Asha', 'ACME', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_app.person WHERE id = md5('e2e-corp-rider-1')::uuid::text);

INSERT INTO atlas_app.person
  (id, blocked, gender, has_taken_valid_ride, identifier_type, is_new, is_valid_rating, role, merchant_id, merchant_operating_city_id,
   mobile_country_code, mobile_number_encrypted, mobile_number_hash, unencrypted_mobile_number, first_name, last_name, created_at, updated_at)
SELECT md5('e2e-corp-rider-2')::uuid::text, false, 'UNKNOWN', false, 'MOBILENUMBER', false, true, 'USER',
       '4b17bd06-ae7e-48e9-85bf-282fb310209c', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874',
       '+91', :'r2_enc', decode(:'r2_hash', 'hex'), '9999900122', 'Bala', 'ACME', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_app.person WHERE id = md5('e2e-corp-rider-2')::uuid::text);

INSERT INTO atlas_app.person
  (id, blocked, gender, has_taken_valid_ride, identifier_type, is_new, is_valid_rating, role, merchant_id, merchant_operating_city_id,
   mobile_country_code, mobile_number_encrypted, mobile_number_hash, unencrypted_mobile_number, first_name, last_name, created_at, updated_at)
SELECT md5('e2e-corp-rider-3')::uuid::text, false, 'UNKNOWN', false, 'MOBILENUMBER', false, true, 'USER',
       '4b17bd06-ae7e-48e9-85bf-282fb310209c', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874',
       '+91', :'r3_enc', decode(:'r3_hash', 'hex'), '9999900123', 'Charu', 'Walkup', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_app.person WHERE id = md5('e2e-corp-rider-3')::uuid::text);

INSERT INTO atlas_app.registration_token
  (id, attempts, auth_expiry, auth_medium, auth_type, auth_value_hash, entity_id, entity_type, merchant_id, token, token_expiry, verified, created_at, updated_at)
SELECT md5('e2e-corp-rider-' || n || '-token')::uuid::text, 0, 365, 'SMS', 'OTP', '7891', md5('e2e-corp-rider-' || n)::uuid::text,
       'USER', '4b17bd06-ae7e-48e9-85bf-282fb310209c', 'e2e-corp-rider-' || n || '-token', 365, true, now(), now()
FROM generate_series(1, 3) AS n
WHERE NOT EXISTS (SELECT 1 FROM atlas_app.registration_token WHERE id = md5('e2e-corp-rider-' || n || '-token')::uuid::text);
