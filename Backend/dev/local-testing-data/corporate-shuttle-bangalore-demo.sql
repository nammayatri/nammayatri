-- ****************** --
-- NOT FOR PRODUCTION --
-- ****************** --

-- Corporate shuttle demo data for LOCAL testing of the control centre's Shuttle Interest page (table, map and published
-- routes), on the local Bangalore NAMMA_YATRI city. LOCAL ONLY. Run by hand after the stack is up:
--   psql -h localhost -p 5434 -U atlas_superuser -d atlas_dev -f Backend/dev/local-testing-data/corporate-shuttle-bangalore-demo.sql
-- Re-running replaces the demo rows (person ids 7e570000-...); other rows are never touched.
--
-- Small on purpose, to show how routes group (Backend/dev/corporate-shuttle-routes/planner.py), all round trips Mon-Fri.
-- Each place is one meeting point with its people scattered up to ~450 m around it (all within walking distance of it),
-- every pickup inside the local road map (Backend/nix/osrm.nix: lat 12.90-13.00, lon 77.55-77.65):
--   Namma Yatri (17): Indiranagar 4 at 08:30 + 3 at 09:30, Domlur (2 km on) 3 at 08:30 + 3 at 09:30
--                     -> one route Indiranagar -> Domlur -> office, shuttles 08:30 (7) and 09:30 (6)
--                     Vittal Mallya Road 4 at 08:30, around a local tester's own pickup (12.9716, 77.5946)
--                     -> with that tester, a route with an 08:30 shuttle (5)
--   Juspay (14):      Jayanagar 7 at 08:30 -> a route with an 08:30 shuttle (7)
--                     Ulsoor 6 at 09:00 + 1 at 09:15 -> a route with one 09:00 shuttle (7): the light 09:15 joins it
--   Evening: the same, leaving the office at the drop times.

-- 1. companies ─────────────────────────────────────────────────────────────────────────────────────────────────────
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

-- 2. demo interest forms ───────────────────────────────────────────────────────────────────────────────────────────
DELETE FROM atlas_app.shuttle_interest WHERE person_id LIKE '7e570000-%';
-- each row takes its company from its email domain, so it is inserted already linked
INSERT INTO atlas_app.shuttle_interest (person_id, company_domain, pickup_lat, pickup_lon, pickup_address, drop_lat, drop_lon, drop_address, trip_type, pickup_time, drop_time, days_of_week, merchant_id, merchant_operating_city_id, created_at, updated_at, pass_organization_id)
SELECT v.person_id, v.company_domain, v.pickup_lat, v.pickup_lon, v.pickup_address, v.drop_lat, v.drop_lon, v.drop_address, 'ROUND_TRIP',
       v.pickup_time::time, v.drop_time::time, '{Monday,Tuesday,Wednesday,Thursday,Friday}', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '36ebf5ac-7916-94c7-4286-3a28dd64cbcc', now(), now(), po.id
FROM (VALUES
  ('7e570000-0000-4000-8000-000000000001', 'nammayatri.in', 12.979000, 77.637000, 'Indiranagar, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', '08:30:00', '18:30:00'),
  ('7e570000-0000-4000-8000-000000000002', 'nammayatri.in', 12.981563, 77.638117, 'Indiranagar, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', '09:30:00', '19:30:00'),
  ('7e570000-0000-4000-8000-000000000003', 'nammayatri.in', 12.979526, 77.638569, 'Indiranagar, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', '08:30:00', '18:30:00'),
  ('7e570000-0000-4000-8000-000000000004', 'nammayatri.in', 12.977229, 77.640419, 'Indiranagar, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', '08:30:00', '18:30:00'),
  ('7e570000-0000-4000-8000-000000000005', 'nammayatri.in', 12.976938, 77.638125, 'Indiranagar, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', '09:30:00', '19:30:00'),
  ('7e570000-0000-4000-8000-000000000006', 'nammayatri.in', 12.975825, 77.635481, 'Indiranagar, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', '08:30:00', '18:30:00'),
  ('7e570000-0000-4000-8000-000000000007', 'nammayatri.in', 12.978509, 77.635812, 'Indiranagar, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', '09:30:00', '19:30:00'),
  ('7e570000-0000-4000-8000-000000000008', 'nammayatri.in', 12.961000, 77.638000, 'Domlur, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', '08:30:00', '18:30:00'),
  ('7e570000-0000-4000-8000-000000000009', 'nammayatri.in', 12.963563, 77.639117, 'Domlur, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', '09:30:00', '19:30:00'),
  ('7e570000-0000-4000-8000-000000000010', 'nammayatri.in', 12.961526, 77.639569, 'Domlur, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', '09:30:00', '19:30:00'),
  ('7e570000-0000-4000-8000-000000000011', 'nammayatri.in', 12.959229, 77.641418, 'Domlur, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', '08:30:00', '18:30:00'),
  ('7e570000-0000-4000-8000-000000000012', 'nammayatri.in', 12.958938, 77.639125, 'Domlur, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', '08:30:00', '18:30:00'),
  ('7e570000-0000-4000-8000-000000000013', 'nammayatri.in', 12.957825, 77.636481, 'Domlur, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', '09:30:00', '19:30:00'),
  ('7e570000-0000-4000-8000-000000000101', 'juspay.in', 12.924500, 77.598000, 'Jayanagar 4th Block, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', '08:30:00', '18:30:00'),
  ('7e570000-0000-4000-8000-000000000102', 'juspay.in', 12.927063, 77.599116, 'Jayanagar 4th Block, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', '08:30:00', '18:30:00'),
  ('7e570000-0000-4000-8000-000000000103', 'juspay.in', 12.925026, 77.599569, 'Jayanagar 4th Block, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', '08:30:00', '18:30:00'),
  ('7e570000-0000-4000-8000-000000000104', 'juspay.in', 12.922729, 77.601418, 'Jayanagar 4th Block, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', '08:30:00', '18:30:00'),
  ('7e570000-0000-4000-8000-000000000105', 'juspay.in', 12.922438, 77.599125, 'Jayanagar 4th Block, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', '08:30:00', '18:30:00'),
  ('7e570000-0000-4000-8000-000000000106', 'juspay.in', 12.921325, 77.596481, 'Jayanagar 4th Block, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', '08:30:00', '18:30:00'),
  ('7e570000-0000-4000-8000-000000000107', 'juspay.in', 12.924009, 77.596812, 'Jayanagar 4th Block, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', '08:30:00', '18:30:00'),
  ('7e570000-0000-4000-8000-000000000108', 'juspay.in', 12.978000, 77.619000, 'Ulsoor, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', '09:00:00', '19:00:00'),
  ('7e570000-0000-4000-8000-000000000109', 'juspay.in', 12.980563, 77.620117, 'Ulsoor, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', '09:00:00', '19:00:00'),
  ('7e570000-0000-4000-8000-000000000110', 'juspay.in', 12.978526, 77.620569, 'Ulsoor, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', '09:15:00', '19:15:00'),
  ('7e570000-0000-4000-8000-000000000111', 'juspay.in', 12.976229, 77.622419, 'Ulsoor, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', '09:00:00', '19:00:00'),
  ('7e570000-0000-4000-8000-000000000112', 'juspay.in', 12.975938, 77.620125, 'Ulsoor, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', '09:00:00', '19:00:00'),
  ('7e570000-0000-4000-8000-000000000113', 'juspay.in', 12.974825, 77.617481, 'Ulsoor, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', '09:00:00', '19:00:00'),
  ('7e570000-0000-4000-8000-000000000114', 'juspay.in', 12.977509, 77.617812, 'Ulsoor, Bengaluru (demo)', 12.9352, 77.6146, 'Office, Koramangala 5th Block', '09:00:00', '19:00:00'),
  ('7e570000-0000-4000-8000-000000000014', 'nammayatri.in', 12.973527, 77.596259, 'Vittal Mallya Road, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', '08:30:00', '18:30:00'),
  ('7e570000-0000-4000-8000-000000000015', 'nammayatri.in', 12.970393, 77.595838, 'Vittal Mallya Road, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', '08:30:00', '18:30:00'),
  ('7e570000-0000-4000-8000-000000000016', 'nammayatri.in', 12.968779, 77.592171, 'Vittal Mallya Road, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', '08:30:00', '18:30:00'),
  ('7e570000-0000-4000-8000-000000000017', 'nammayatri.in', 12.973505, 77.592270, 'Vittal Mallya Road, Bengaluru (demo)', 12.9279, 77.6271, 'Office, Koramangala', '08:30:00', '18:30:00')
) AS v (person_id, company_domain, pickup_lat, pickup_lon, pickup_address, drop_lat, drop_lon, drop_address, pickup_time, drop_time)
JOIN atlas_app.pass_organization po ON po.merchant_operating_city_id = '36ebf5ac-7916-94c7-4286-3a28dd64cbcc' AND v.company_domain = ANY (po.email_domains);
