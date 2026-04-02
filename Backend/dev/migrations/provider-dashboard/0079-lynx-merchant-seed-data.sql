-- WARNING: LOCAL DEVELOPMENT ONLY — DO NOT RUN IN PRODUCTION
-- LYNX_PARTNER dashboard merchant seed data for fleet onboarding with admin approval

DO $$
DECLARE
  v_lynx_dashboard_merchant_id TEXT;
  v_lynx_admin_person_id TEXT := 'lynx-admin-person-0000-000000000000';
  v_admin_role_id TEXT := '37947162-3b5d-4ed6-bcac-08841be1534d';   -- JUSPAY_ADMIN role
BEGIN
  -- Safety check
  IF current_database() NOT IN ('atlas_dev', 'atlas_dev_test', 'nammayatri_dev') THEN
    RAISE NOTICE 'Skipping Lynx dashboard seed: database "%" does not look like a local dev DB.', current_database();
    RETURN;
  END IF;

  -- Skip if LYNX already exists
  IF EXISTS (SELECT 1 FROM atlas_bpp_dashboard.merchant WHERE short_id = 'LYNX_PARTNER') THEN
    RAISE NOTICE 'LYNX_PARTNER dashboard merchant already exists — skipping.';
    RETURN;
  END IF;

  -- BPP Dashboard merchant — admin approval required for fleet onboarding
  INSERT INTO atlas_bpp_dashboard.merchant (id, short_id, server_name, server_names, created_at, is2fa_mandatory, default_operating_city, supported_operating_cities, domain, enabled, require_admin_approval_for_fleet_onboarding, is_strong_name_check_required, verify_fleet_while_login)
  SELECT md5('LYNX_PARTNER'), 'LYNX_PARTNER', server_name, server_names, now(), is2fa_mandatory, 'Helsinki', '{Helsinki}', domain, true, true, false, false
  FROM atlas_bpp_dashboard.merchant WHERE short_id = 'NAMMA_YATRI_PARTNER'
  ON CONFLICT DO NOTHING;

  v_lynx_dashboard_merchant_id := md5('LYNX_PARTNER');

  -- Admin user for Lynx (email: admin@lynx.test, password: lynx1234)
  INSERT INTO atlas_bpp_dashboard.person (id, first_name, last_name, email_encrypted, email_hash,
    mobile_number_encrypted, mobile_number_hash, mobile_country_code, password_hash,
    created_at, updated_at, role_id, dashboard_access_type)
  VALUES (
    v_lynx_admin_person_id,
    'lynx_admin', 'lynx_admin',
    '0.1.0|4|KGE7MrEduYYPJ1PG2lVTIKhIEWc0K8FZP9ZZ54H+8gqILGxnROmhgSSJwLFIQ88D4bM62BKhPSGgbradhcx1',
    '\x690322684007a0024f54653be2c52249d1b7dd661f91c630118e9e6bd1ba1eaa',
    '0.1.0|5|MiPzhelH9UXkF2mTfSlDqC0/QIm/nAsOlu024RoWcAmjKYXj2Xi7M01xCmC33c6C3l4FLp6Yx5rvWw==',
    '\xe70d8200dff37e3d572c7f9ed23afd1cece4c8fe5cb9f167e57daad9c86ba6f9',
    '+91',
    '\xc42a05ace541233d26bf871345bfc00bfc117878a48bb5c80433347438139a7a',
    now(), now(),
    v_admin_role_id,
    'DASHBOARD_ADMIN'
  ) ON CONFLICT DO NOTHING;

  -- Admin merchant_access for Lynx
  INSERT INTO atlas_bpp_dashboard.merchant_access (id, person_id, merchant_id, merchant_short_id, operating_city, created_at, is2fa_enabled)
  VALUES (
    md5('lynx-admin-merchant-access'),
    v_lynx_admin_person_id,
    v_lynx_dashboard_merchant_id,
    'LYNX_PARTNER',
    'Helsinki',
    now(),
    false
  ) ON CONFLICT DO NOTHING;

  -- Access matrix: admin can fetch unverified accounts and verify/approve them
  INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_action_type, user_access_type, created_at, updated_at)
  VALUES
    (md5('lynx-admin-fetch-unverified'), v_admin_role_id, 'DSL', 'PROVIDER_MANAGEMENT/ACCOUNT/GET_ACCOUNT_FETCH_UNVERIFIED_ACCOUNTS', 'USER_FULL_ACCESS', now(), now()),
    (md5('lynx-admin-verify-account'), v_admin_role_id, 'DSL', 'PROVIDER_MANAGEMENT/ACCOUNT/POST_ACCOUNT_VERIFY_ACCOUNT', 'USER_FULL_ACCESS', now(), now())
  ON CONFLICT DO NOTHING;

  -- Fleet role access for onboarding + registration
  INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_action_type, user_access_type, created_at, updated_at)
  VALUES
    (md5('lynx-fleet-onboard-configs'), 'e5a69a26-d165-455a-a711-33a41e0d4812', 'DSL', 'PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_DOCUMENT_CONFIGS', 'USER_FULL_ACCESS', now(), now()),
    (md5('lynx-fleet-onboard-status'), 'e5a69a26-d165-455a-a711-33a41e0d4812', 'DSL', 'PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_REGISTER_STATUS', 'USER_FULL_ACCESS', now(), now()),
    (md5('lynx-fleet-onboard-verify'), 'e5a69a26-d165-455a-a711-33a41e0d4812', 'DSL', 'PROVIDER_FLEET/ONBOARDING/POST_ONBOARDING_VERIFY', 'USER_FULL_ACCESS', now(), now()),
    (md5('lynx-fleet-onboard-referral'), 'e5a69a26-d165-455a-a711-33a41e0d4812', 'DSL', 'PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_GET_REFERRAL_DETAILS', 'USER_FULL_ACCESS', now(), now()),
    (md5('lynx-fleet-register-v2'), 'e5a69a26-d165-455a-a711-33a41e0d4812', 'DSL', 'PROVIDER_FLEET/REGISTRATION_V2/POST_REGISTRATION_V2_REGISTER', 'USER_FULL_ACCESS', now(), now())
  ON CONFLICT DO NOTHING;

  RAISE NOTICE 'LYNX_PARTNER dashboard merchant + admin + access matrix created.';
END;
$$;
