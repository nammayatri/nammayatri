-- ****************** --
-- NOT FOR PRODUCTION --
-- ****************** --

-- Corporate shuttle demo data for LOCAL testing of the control centre's Shuttle Interest page (table, map and
-- "Suggested routes"), on the local Bangalore NAMMA_YATRI city. LOCAL ONLY. Run by hand after the stack is up:
--   psql -h localhost -p 5434 -U atlas_superuser -d atlas_dev -f Backend/dev/local-testing-data/corporate-shuttle-bangalore-demo.sql
-- then restart rider-app (or clear its Redis) so it reads the new rows. Idempotent: re-running changes nothing.
--
-- It does five things:
--   1. adds the module C columns if the local DB does not have them yet (pass_organization.email_domains,
--      shuttle_interest.pass_organization_id; the generated migrations are in dev/migrations-read-only/rider-app/)
--   2. creates the two companies as corporate pass organisations: Namma Yatri (nammayatri.in) and Juspay (juspay.in)
--   3. adds demo interest forms (person ids 7e570000-0000-4000-8000-0000000001xx) so each company has about 20,
--      clustered by area so routes form; rows already there are never touched
--   4. links every interest row to its company by its email domain
--   5. moves every demo row inside the local road map (central Bangalore, Backend/nix/osrm.nix indiaBbox), so routes
--      follow real roads

-- 1. columns ───────────────────────────────────────────────────────────────────────────────────────────────────────
ALTER TABLE atlas_app.pass_organization ADD COLUMN IF NOT EXISTS email_domains text[];
ALTER TABLE atlas_app.pass_organization ADD COLUMN IF NOT EXISTS office_lat double precision;
ALTER TABLE atlas_app.pass_organization ADD COLUMN IF NOT EXISTS office_lon double precision;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN IF NOT EXISTS pass_organization_id character varying(36);

-- 2. companies ─────────────────────────────────────────────────────────────────────────────────────────────────────
-- person_id is the organisation's admin person in the pass dashboards; a placeholder id is enough for the shuttle.
INSERT INTO atlas_app.pass_organization (id, name, pass_enum, person_id, merchant_id, merchant_operating_city_id, address, email_domains, created_at, updated_at) VALUES
  (md5('demo-corp-org-nammayatri')::uuid::text, 'Namma Yatri', 'RegularPass', md5('demo-corp-org-admin-nammayatri')::uuid::text, '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', 'Office, Koramangala, Bengaluru', '{nammayatri.in}', now(), now()),
  (md5('demo-corp-org-juspay')::uuid::text, 'Juspay', 'RegularPass', md5('demo-corp-org-admin-juspay')::uuid::text, '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', 'Office, Koramangala 5th Block, Bengaluru', '{juspay.in}', now(), now())
ON CONFLICT DO NOTHING;
UPDATE atlas_app.pass_organization SET email_domains = '{nammayatri.in}' WHERE id = md5('demo-corp-org-nammayatri')::uuid::text;
UPDATE atlas_app.pass_organization SET email_domains = '{juspay.in}' WHERE id = md5('demo-corp-org-juspay')::uuid::text;
-- the offices (the drop of every to-office trip): the points the demo interest forms already drop at
UPDATE atlas_app.pass_organization SET office_lat = 12.9279, office_lon = 77.6271 WHERE id = md5('demo-corp-org-nammayatri')::uuid::text;
UPDATE atlas_app.pass_organization SET office_lat = 12.9352, office_lon = 77.6146 WHERE id = md5('demo-corp-org-juspay')::uuid::text;

-- 3. demo interest forms ───────────────────────────────────────────────────────────────────────────────────────────
-- each row takes its company from its email domain, so it is inserted already linked
INSERT INTO atlas_app.shuttle_interest (person_id, company_domain, pickup_lat, pickup_lon, pickup_address, drop_lat, drop_lon, drop_address, trip_type, pickup_time, drop_time, shift_start_time, shift_end_time, max_walking_distance_meters, days_of_week, time_flexibility_minutes, ac_preference, vehicle_size_preference, merchant_id, merchant_operating_city_id, created_at, updated_at, pass_organization_id)
SELECT v.person_id, v.company_domain, v.pickup_lat, v.pickup_lon, v.pickup_address, v.drop_lat, v.drop_lon, v.drop_address, v.trip_type,
       v.pickup_time::time, v.drop_time::time, v.shift_start_time::time, v.shift_end_time::time, v.max_walking_distance_meters,
       v.days_of_week::text[], v.time_flexibility_minutes, v.ac_preference, v.vehicle_size_preference, v.merchant_id,
       v.merchant_operating_city_id, v.created_at, v.updated_at, po.id
FROM (VALUES
  ('7e570000-0000-4000-8000-000000000101', 'nammayatri.in', 12.910633, 77.646108, 'HSR Layout, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', 'ROUND_TRIP', '08:15:00', '18:30:00', '09:30:00', '18:30:00', 500, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 15, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000102', 'nammayatri.in', 12.911085, 77.649129, 'HSR Layout, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', 'ROUND_TRIP', '08:30:00', '18:30:00', '09:30:00', '18:30:00', 700, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 30, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000103', 'nammayatri.in', 12.911593, 77.647322, 'HSR Layout, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', 'TO_OFFICE', '08:30:00', NULL, '09:30:00', '18:30:00', 500, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 15, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000104', 'nammayatri.in', 12.957328, 77.698339, 'Marathahalli, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', 'ROUND_TRIP', '08:45:00', '19:00:00', '09:30:00', '18:30:00', 1000, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 15, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000105', 'nammayatri.in', 12.959079, 77.695798, 'Marathahalli, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', 'TO_HOME', NULL, '18:15:00', '09:30:00', '18:30:00', 400, '{Monday,Tuesday,Wednesday,Thursday,Friday,Saturday}', 30, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000106', 'nammayatri.in', 12.968734, 77.751334, 'Whitefield, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', 'ROUND_TRIP', '08:30:00', '19:15:00', '09:30:00', '18:30:00', 600, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 15, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000107', 'nammayatri.in', 12.968494, 77.750713, 'Whitefield, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', 'TO_OFFICE', '08:15:00', NULL, '09:30:00', '18:30:00', 500, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 30, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000108', 'nammayatri.in', 12.980087, 77.641174, 'Indiranagar, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', 'ROUND_TRIP', '08:30:00', '18:30:00', '09:30:00', '18:30:00', 700, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 15, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000109', 'nammayatri.in', 12.977885, 77.639169, 'Indiranagar, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', 'ROUND_TRIP', '08:30:00', '18:45:00', '09:30:00', '18:30:00', 500, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 15, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000110', 'nammayatri.in', 12.844868, 77.660772, 'Electronic City, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', 'TO_OFFICE', '08:45:00', NULL, '09:30:00', '18:30:00', 1000, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 30, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000111', 'nammayatri.in', 12.843584, 77.661322, 'Electronic City, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', 'ROUND_TRIP', '09:00:00', '18:15:00', '09:30:00', '18:30:00', 400, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 15, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000112', 'nammayatri.in', 12.925529, 77.595473, 'Jayanagar, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', 'TO_HOME', NULL, '19:15:00', '09:30:00', '18:30:00', 600, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 30, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000141', 'juspay.in', 13.034494, 77.595666, 'Hebbal, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', 'ROUND_TRIP', '08:15:00', '18:30:00', '09:30:00', '18:30:00', 500, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 15, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000142', 'juspay.in', 13.037162, 77.595567, 'Hebbal, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', 'ROUND_TRIP', '08:30:00', '18:30:00', '09:30:00', '18:30:00', 700, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 30, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000143', 'juspay.in', 12.916889, 77.610516, 'BTM Layout, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', 'TO_OFFICE', '08:30:00', NULL, '09:30:00', '18:30:00', 500, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 15, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000144', 'juspay.in', 12.915647, 77.610813, 'BTM Layout, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', 'ROUND_TRIP', '08:45:00', '19:00:00', '09:30:00', '18:30:00', 1000, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 15, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000145', 'juspay.in', 12.915972, 77.608992, 'BTM Layout, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', 'TO_HOME', NULL, '18:15:00', '09:30:00', '18:30:00', 400, '{Monday,Tuesday,Wednesday,Thursday,Friday,Saturday}', 30, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000146', 'juspay.in', 12.905926, 77.587218, 'JP Nagar, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', 'ROUND_TRIP', '08:30:00', '19:15:00', '09:30:00', '18:30:00', 600, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 15, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000147', 'juspay.in', 12.905262, 77.584648, 'JP Nagar, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', 'TO_OFFICE', '08:15:00', NULL, '09:30:00', '18:30:00', 500, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 30, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000148', 'juspay.in', 12.926687, 77.592508, 'Jayanagar, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', 'ROUND_TRIP', '08:30:00', '18:30:00', '09:30:00', '18:30:00', 700, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 15, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000149', 'juspay.in', 12.925656, 77.592409, 'Jayanagar, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', 'ROUND_TRIP', '08:30:00', '18:45:00', '09:30:00', '18:30:00', 500, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 15, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000150', 'juspay.in', 12.971596, 77.595218, 'MG Road, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', 'TO_OFFICE', '08:45:00', NULL, '09:30:00', '18:30:00', 1000, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 30, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000151', 'juspay.in', 13.027259, 77.538313, 'Yeshwanthpur, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', 'ROUND_TRIP', '09:00:00', '18:15:00', '09:30:00', '18:30:00', 400, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 15, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000152', 'juspay.in', 13.027414, 77.539993, 'Yeshwanthpur, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', 'TO_HOME', NULL, '19:15:00', '09:30:00', '18:30:00', 600, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 30, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000153', 'juspay.in', 12.902024, 77.696501, 'Sarjapur Road, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', 'ROUND_TRIP', '08:15:00', '18:30:00', '09:30:00', '18:30:00', 500, '{Monday,Tuesday,Wednesday,Thursday,Friday}', 15, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now()),
  ('7e570000-0000-4000-8000-000000000154', 'juspay.in', 12.901487, 77.696431, 'Sarjapur Road, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', 'TO_OFFICE', '08:30:00', NULL, '09:30:00', '18:30:00', 700, '{Monday,Tuesday,Wednesday,Thursday,Friday,Saturday}', 15, 'AC_PREFERRED', 'MINI_BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now())
) AS v(person_id, company_domain, pickup_lat, pickup_lon, pickup_address, drop_lat, drop_lon, drop_address, trip_type, pickup_time, drop_time, shift_start_time, shift_end_time, max_walking_distance_meters, days_of_week, time_flexibility_minutes, ac_preference, vehicle_size_preference, merchant_id, merchant_operating_city_id, created_at, updated_at)
JOIN atlas_app.pass_organization po ON po.merchant_operating_city_id = v.merchant_operating_city_id AND v.company_domain = ANY (po.email_domains)
ON CONFLICT (person_id) DO NOTHING;

-- 4. link interests to their company ───────────────────────────────────────────────────────────────────────────────
UPDATE atlas_app.shuttle_interest si SET pass_organization_id = po.id
FROM atlas_app.pass_organization po
WHERE si.pass_organization_id IS NULL AND po.merchant_operating_city_id = si.merchant_operating_city_id AND si.company_domain = ANY (po.email_domains);

-- the generated migration declares the column NOT NULL; apply that once every row has a company
DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM atlas_app.shuttle_interest WHERE pass_organization_id IS NULL) THEN
    ALTER TABLE atlas_app.shuttle_interest ALTER COLUMN pass_organization_id SET NOT NULL;
  END IF;
END $$;

-- 5. keep the demo inside the local road map ────────────────────────────────────────────────────────────────────────
-- The local OSRM (Backend/nix/osrm.nix, indiaBbox) only has central Bangalore's roads: lon 77.55-77.65, lat 12.90-13.00.
-- Every demo row (person ids 7e570000-...) gets a pickup point inside it, so its stops snap to real roads. Each row keeps
-- its company, trip type, times, days and walking limit. Seshadripuram is a dense far cluster (5 people, both companies),
-- Frazer Town a lone far employee.
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.912042, pickup_lon = 77.640287, pickup_address = 'HSR Layout Sector 1, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000001';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.912315, pickup_lon = 77.641068, pickup_address = 'HSR Layout Sector 1, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000002';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.911449, pickup_lon = 77.639976, pickup_address = 'HSR Layout Sector 1, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000003';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.912240, pickup_lon = 77.639591, pickup_address = 'HSR Layout Sector 1, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000004';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.907432, pickup_lon = 77.622261, pickup_address = 'Bommanahalli, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000005';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.907300, pickup_lon = 77.624040, pickup_address = 'Bommanahalli, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000006';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.979026, pickup_lon = 77.637101, pickup_address = 'Indiranagar, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000101';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.978772, pickup_lon = 77.637995, pickup_address = 'Indiranagar, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000102';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.979525, pickup_lon = 77.636856, pickup_address = 'Indiranagar, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000103';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.961348, pickup_lon = 77.637739, pickup_address = 'Domlur, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000104';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.960482, pickup_lon = 77.638304, pickup_address = 'Domlur, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000105';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.944167, pickup_lon = 77.626136, pickup_address = 'Ejipura, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000106';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.943800, pickup_lon = 77.626118, pickup_address = 'Ejipura, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000107';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.981861, pickup_lon = 77.621087, pickup_address = 'Ulsoor, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000108';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.980741, pickup_lon = 77.620071, pickup_address = 'Ulsoor, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000109';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.992289, pickup_lon = 77.614986, pickup_address = 'Frazer Town, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000110';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.988854, pickup_lon = 77.573384, pickup_address = 'Seshadripuram, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000111';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.988892, pickup_lon = 77.575115, pickup_address = 'Seshadripuram, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000112';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.916624, pickup_lon = 77.610754, pickup_address = 'BTM Layout, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000011';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.916313, pickup_lon = 77.609869, pickup_address = 'BTM Layout, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000012';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.915607, pickup_lon = 77.611112, pickup_address = 'BTM Layout, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000013';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.917800, pickup_lon = 77.609860, pickup_address = 'BTM Layout, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000014';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.924779, pickup_lon = 77.593155, pickup_address = 'Jayanagar 4th Block, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000015';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.923856, pickup_lon = 77.594925, pickup_address = 'Jayanagar 4th Block, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000016';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.924120, pickup_lon = 77.593664, pickup_address = 'Jayanagar 4th Block, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000141';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.911842, pickup_lon = 77.585221, pickup_address = 'JP Nagar 2nd Phase, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000142';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.911325, pickup_lon = 77.585682, pickup_address = 'JP Nagar 2nd Phase, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000143';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.910308, pickup_lon = 77.586002, pickup_address = 'JP Nagar 2nd Phase, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000144';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.942876, pickup_lon = 77.572942, pickup_address = 'Basavanagudi, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000145';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.941060, pickup_lon = 77.573215, pickup_address = 'Basavanagudi, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000146';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.946913, pickup_lon = 77.597579, pickup_address = 'Wilson Garden, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000147';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.948108, pickup_lon = 77.597165, pickup_address = 'Wilson Garden, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000148';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.958052, pickup_lon = 77.600936, pickup_address = 'Shanthinagar, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000149';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.957722, pickup_lon = 77.600795, pickup_address = 'Shanthinagar, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000150';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.975271, pickup_lon = 77.607861, pickup_address = 'MG Road, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000151';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.989184, pickup_lon = 77.572838, pickup_address = 'Seshadripuram, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000152';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.989362, pickup_lon = 77.573685, pickup_address = 'Seshadripuram, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000153';
UPDATE atlas_app.shuttle_interest SET pickup_lat = 12.987951, pickup_lon = 77.574259, pickup_address = 'Seshadripuram, Bengaluru (demo)', updated_at = now() WHERE person_id = '7e570000-0000-4000-8000-000000000154';
