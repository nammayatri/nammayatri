-- ============================================================================
-- Phase 1 / 0006: merge merchants + seed merchant_pair.
-- Short-id sets are disjoint (phase 0, see 0000), so this is a plain union; BAP lacks
-- company_name / email_* / password_hash columns, hence the narrower list.
-- ============================================================================

-- 4.1 BPP merchants. SELECT * is safe: the merged table shell was created
-- via LIKE atlas_bpp_dashboard.merchant (0002), so the column sets match
-- in every environment even where schemas have drifted (e.g. local dev
-- lacks company_name/email_*/two_factor_mandatory_for_roles).
INSERT INTO atlas_dashboard.merchant
SELECT * FROM atlas_bpp_dashboard.merchant;

-- 4.2 BAP merchants (narrower list; BPP-only columns default to NULL).
INSERT INTO atlas_dashboard.merchant
  (id, short_id, server_name, created_at, default_operating_city,
   supported_operating_cities, server_names, domain, website, enabled,
   auth_token_encrypted, auth_token_hash,
   require_admin_approval_for_fleet_onboarding, has_fleet_member_hierarchy,
   is_strong_name_check_required, verify_fleet_while_login,
   single_active_session_only, track_login_logout_for_roles)
SELECT id, short_id, server_name, created_at, default_operating_city,
       supported_operating_cities, server_names, domain, website, enabled,
       auth_token_encrypted, auth_token_hash,
       require_admin_approval_for_fleet_onboarding, has_fleet_member_hierarchy,
       is_strong_name_check_required, verify_fleet_while_login,
       single_active_session_only, track_login_logout_for_roles
FROM atlas_bap_dashboard.merchant;

-- 4.3 merchant_pair: the logical merchant the UI selects, resolving to a
-- per-platform id. Pairs per the 2026-08-05 prod exports; NULL side = the
-- merchant exists on one platform only.
INSERT INTO atlas_dashboard.merchant_pair (logical_short_id, bap_merchant_id, bpp_merchant_id)
SELECT v.logical, bap.id, bpp.id
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
LEFT JOIN atlas_bap_dashboard.merchant bap ON bap.short_id = v.bap_short
LEFT JOIN atlas_bpp_dashboard.merchant bpp ON bpp.short_id = v.bpp_short;

-- 4.4 Guard: every source merchant is paired. MUST return 0.
SELECT count(*) AS unpaired_merchants FROM atlas_dashboard.merchant m
WHERE m.id NOT IN (
  SELECT bap_merchant_id FROM atlas_dashboard.merchant_pair WHERE bap_merchant_id IS NOT NULL
  UNION
  SELECT bpp_merchant_id FROM atlas_dashboard.merchant_pair WHERE bpp_merchant_id IS NOT NULL);
