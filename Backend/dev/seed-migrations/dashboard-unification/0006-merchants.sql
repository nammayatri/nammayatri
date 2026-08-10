-- ============================================================================
-- Phase 1 / 0006: merge merchants + seed merchant_pair.
-- Short-id sets are disjoint (phase 0, see 0000), so this is a plain union; BAP lacks
-- company_name / email_* / password_hash columns, hence the narrower list.
-- ============================================================================

-- 4.1 BPP merchants. SELECT * is safe: the merged table shell was created
-- via LIKE atlas_dashboard.bpp_merchant (0002), so the column sets match
-- in every environment even where schemas have drifted (e.g. local dev
-- lacks company_name/email_*/two_factor_mandatory_for_roles).
INSERT INTO atlas_dashboard.merchant
SELECT * FROM atlas_dashboard.bpp_merchant;

-- 4.1b Map every BAP merchant to the id it will have in the merged table.
-- Non-colliding ids are kept; a collision with a BPP merchant gets a new,
-- deterministic id derived from the short_id (stable across reruns and
-- environments) so both merchants survive.
INSERT INTO atlas_dashboard.legacy_bap_merchant (bap_merchant_id, merchant_id, re_ided)
SELECT b.id,
       CASE WHEN EXISTS (SELECT 1 FROM atlas_dashboard.merchant m WHERE m.id = b.id)
            THEN md5('bap-merchant:' || b.short_id)::uuid::text
            ELSE b.id END,
       EXISTS (SELECT 1 FROM atlas_dashboard.merchant m WHERE m.id = b.id)
FROM atlas_dashboard.bap_merchant b
ON CONFLICT DO NOTHING;

-- Report the re-ided merchants: their BAP-side references are rewritten in
-- 0007/0008, so anything outside this migration that stored the old id (none
-- known) would need the same treatment.
SELECT map.bap_merchant_id AS old_id, map.merchant_id AS new_id, b.short_id
FROM atlas_dashboard.legacy_bap_merchant map
JOIN atlas_dashboard.bap_merchant b ON b.id = map.bap_merchant_id
WHERE map.re_ided;

--                 old_id                |                new_id                |   short_id
-- --------------------------------------+--------------------------------------+--------------
--  b7269e46-933a-40c0-b636-7903d29a31b4 | 459da2b2-b38e-b5c9-717f-43fad4670e3f | BRIDGE_CABS
--  2e8eac28-9854-4f5d-aea6-a2f6502cfe37 | ffa475ae-0ad2-7f75-b990-b5e5104876b7 | JATRI_SAATHI
--  94bbea0d-3c52-479b-81f5-eca4969ae797 | a144d6d4-8ad6-1b76-d916-67a4ea16c46d | NAMMA_YATRI

-- 4.2 BAP merchants (narrower list; BPP-only columns default to NULL).
INSERT INTO atlas_dashboard.merchant
  (id, short_id, server_name, created_at, default_operating_city,
   supported_operating_cities, server_names, domain, website, enabled,
   auth_token_encrypted, auth_token_hash,
   require_admin_approval_for_fleet_onboarding, has_fleet_member_hierarchy,
   is_strong_name_check_required, verify_fleet_while_login,
   single_active_session_only, track_login_logout_for_roles)
SELECT map.merchant_id, b.short_id, b.server_name, b.created_at, b.default_operating_city,
       b.supported_operating_cities, b.server_names, b.domain, b.website, b.enabled,
       b.auth_token_encrypted, b.auth_token_hash,
       b.require_admin_approval_for_fleet_onboarding, b.has_fleet_member_hierarchy,
       b.is_strong_name_check_required, b.verify_fleet_while_login,
       b.single_active_session_only, b.track_login_logout_for_roles
FROM atlas_dashboard.bap_merchant b
JOIN atlas_dashboard.legacy_bap_merchant map ON map.bap_merchant_id = b.id;

-- 4.3 merchant_pair: the logical merchant the UI selects, resolving to a
-- per-platform id. Pairs per the 2026-08-05 prod exports; NULL side = the
-- merchant exists on one platform only.
INSERT INTO atlas_dashboard.merchant_pair (logical_short_id, bap_merchant_id, bpp_merchant_id)
SELECT v.logical, bap.merchant_id, bpp.id
FROM (VALUES
    ('NAMMA_YATRI',    'NAMMA_YATRI',    'NAMMA_YATRI_PARTNER'),
    ('YATRI',          'YATRI',          'YATRI_PARTNER'),
    ('JATRI_SAATHI',   'JATRI_SAATHI',   'JATRI_SAATHI_PARTNER'),
    ('BRIDGE_CABS',    'BRIDGE_CABS',    'BRIDGE_CABS_PARTNER'),
    ('BRIDGE_FINLAND', 'BRIDGE_FINLAND', 'BRIDGE_FINLAND_PARTNER'),
    ('MEGHALAYA_ONE',  'MEGHALAYA_ONE',  'MEGHALAYA_ONE_PARTNER'),
    ('BHARAT_TAXI',    'BHARAT_TAXI',    'BHARAT_TAXI_PARTNER'),
    ('ANNA_APP',       'ANNA_APP',       NULL),
    ('MOBILITY_PAYTM', 'MOBILITY_PAYTM', NULL),
    ('MOBILITY_REDBUS','MOBILITY_REDBUS',NULL),
    ('ADMINISTRATOR',  NULL,             'ADMINISTRATOR')
) AS v(logical, bap_short, bpp_short)
LEFT JOIN atlas_dashboard.bap_merchant bap_src ON bap_src.short_id = v.bap_short
LEFT JOIN atlas_dashboard.legacy_bap_merchant bap ON bap.bap_merchant_id = bap_src.id
LEFT JOIN atlas_dashboard.bpp_merchant bpp ON bpp.short_id = v.bpp_short;

-- 4.4 Guard: every source merchant is paired. MUST return 0.
SELECT count(*) AS unpaired_merchants FROM atlas_dashboard.merchant m
WHERE m.id NOT IN (
  SELECT bap_merchant_id FROM atlas_dashboard.merchant_pair WHERE bap_merchant_id IS NOT NULL
  UNION
  SELECT bpp_merchant_id FROM atlas_dashboard.merchant_pair WHERE bpp_merchant_id IS NOT NULL);

-- ---------------------------------------------------------------------------
-- 4.5 merchant_operating_city: the id -> city/STD-code lookup that
-- Kernel.Storage.Queries.MerchantOperatingCity reads. Reference data, present
-- on both sides. BPP wins; a BAP row is taken only when neither its id nor its
-- city is already present, so the city picker cannot end up with duplicates.
INSERT INTO atlas_dashboard.merchant_operating_city (id, city, std_code)
SELECT id, city, std_code FROM atlas_dashboard.bpp_merchant_operating_city b
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.merchant_operating_city d WHERE d.id = b.id);

INSERT INTO atlas_dashboard.merchant_operating_city (id, city, std_code)
SELECT a.id, a.city, a.std_code FROM atlas_dashboard.bap_merchant_operating_city a
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.merchant_operating_city d
                  WHERE d.id = a.id OR d.city = a.city);

-- Cities present on one side only, for a sanity read.
SELECT d.id, d.city,
       (d.id IN (SELECT id FROM atlas_dashboard.bpp_merchant_operating_city)) AS from_bpp,
       (d.id IN (SELECT id FROM atlas_dashboard.bap_merchant_operating_city)) AS from_bap
FROM atlas_dashboard.merchant_operating_city d ORDER BY d.city;
