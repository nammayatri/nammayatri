-- Local dev merchants for the unified dashboard (atlas_dashboard).
--
-- `local-testing-data/provider-dashboard.sql` seeds PEOPLE and tokens but
-- assumes merchants already exist — every insert there is
-- `SELECT ... FROM atlas_dashboard.merchant WHERE short_id = '...'`, which
-- quietly does nothing on an empty merchant table. That is what a fresh
-- atlas_dashboard looks like after the schema rename, so admin logins come up
-- with no merchants and no access.
--
-- This file creates the merchants those seeds expect. Run it BEFORE
-- provider-dashboard.sql (or re-run that file afterwards). Idempotent.
--
-- Both sides of a logical merchant are created, because the unified server
-- serves both route trees:
--   NAMMA_YATRI          BAP / rider-app     short id
--   NAMMA_YATRI_PARTNER  BPP / driver-app    short id
-- and merchant_pair links them, which cross-tree auth depends on.

-- ---------------------------------------------------------------------------
-- 1. Merchants. Fixed UUIDs so re-runs are stable and tokens keep working.
--
-- server_names drives platform routing. The BPP row lists the APP_BACKEND
-- entries too, mirroring production, so local reproduces the cross-tree
-- behaviour rather than hiding it.
INSERT INTO atlas_dashboard.merchant
  (id, short_id, server_name, created_at, default_operating_city,
   supported_operating_cities, server_names, domain, website, enabled)
VALUES
  ('7f1c9b40-0000-4000-8000-00000000ba91', 'NAMMA_YATRI', 'APP_BACKEND', now(),
   'Bangalore', ARRAY['Bangalore','Kolkata'],
   ARRAY['APP_BACKEND','APP_BACKEND_MANAGEMENT'],
   'localhost', 'http://localhost', true),
  ('7f1c9b40-0000-4000-8000-00000000bb92', 'NAMMA_YATRI_PARTNER', 'DRIVER_OFFER_BPP', now(),
   'Bangalore', ARRAY['Bangalore','Kolkata'],
   ARRAY['DRIVER_OFFER_BPP','DRIVER_OFFER_BPP_MANAGEMENT','APP_BACKEND','APP_BACKEND_MANAGEMENT'],
   'localhost', 'http://localhost', true)
ON CONFLICT (short_id) DO NOTHING;

-- ---------------------------------------------------------------------------
-- 2. Operating cities. Read by Kernel.Storage.Queries.MerchantOperatingCity;
-- without these the city picker is empty and some reads 500.
INSERT INTO atlas_dashboard.merchant_operating_city (id, city, std_code)
SELECT v.id, v.city, v.std_code
FROM (VALUES
    ('7f1c9b40-0000-4000-8000-0000000c1701', 'Bangalore', 'std:080'),
    ('7f1c9b40-0000-4000-8000-0000000c1702', 'Kolkata',   'std:033')
) AS v(id, city, std_code)
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_dashboard.merchant_operating_city e WHERE e.city = v.city);

-- ---------------------------------------------------------------------------
-- 3. Pair the two sides. verifyServerWithPair and the pair-aware
-- merchantCityAccessCheck both read this; without it, BAP-tree calls made with
-- a BPP token are denied.
INSERT INTO atlas_dashboard.merchant_pair (logical_short_id, bap_merchant_id, bpp_merchant_id)
SELECT 'NAMMA_YATRI',
       (SELECT id FROM atlas_dashboard.merchant WHERE short_id = 'NAMMA_YATRI'),
       (SELECT id FROM atlas_dashboard.merchant WHERE short_id = 'NAMMA_YATRI_PARTNER')
ON CONFLICT (logical_short_id) DO NOTHING;

-- ---------------------------------------------------------------------------
-- 4. Grant every JUSPAY_ADMIN access to every merchant x city.
--
-- Keyed off the ROLE, not a hardcoded person id — provider-dashboard.sql
-- grants only '3680f4b5-dce4-4d03-aa8c-5405690e87bd', so any other local admin
-- (one you created, or one a merge produced) silently gets nothing.
INSERT INTO atlas_dashboard.merchant_access
  (id, person_id, merchant_id, merchant_short_id, operating_city, created_at)
SELECT gen_random_uuid()::text, p.id, m.id, m.short_id, c.city, now()
FROM atlas_dashboard.person p
JOIN atlas_dashboard.role r ON r.id = p.role_id AND r.name = 'JUSPAY_ADMIN'
CROSS JOIN atlas_dashboard.merchant m
CROSS JOIN LATERAL unnest(m.supported_operating_cities) AS c(city)
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_dashboard.merchant_access ma
  WHERE ma.person_id = p.id AND ma.merchant_id = m.id
    AND ma.operating_city::text = c.city);

-- ---------------------------------------------------------------------------
-- 5. Check. Expect 2 merchants, 2 cities, 1 pair with both sides, and one
-- access row per admin x merchant x city (4 per admin).
SELECT 'merchants' AS what, count(*)::text AS n FROM atlas_dashboard.merchant
UNION ALL SELECT 'operating cities', count(*)::text FROM atlas_dashboard.merchant_operating_city
UNION ALL SELECT 'pairs (both sides set)', count(*)::text FROM atlas_dashboard.merchant_pair
  WHERE bap_merchant_id IS NOT NULL AND bpp_merchant_id IS NOT NULL
UNION ALL SELECT 'juspay_admin access rows', count(*)::text
  FROM atlas_dashboard.merchant_access ma
  JOIN atlas_dashboard.person p ON p.id = ma.person_id
  JOIN atlas_dashboard.role r ON r.id = p.role_id AND r.name = 'JUSPAY_ADMIN';
