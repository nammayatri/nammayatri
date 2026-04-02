-- WARNING: LOCAL DEVELOPMENT ONLY — DO NOT RUN IN PRODUCTION
-- MSIL_PARTNER dashboard merchant seed data for fleet onboarding testing

DO $$
BEGIN
  -- Safety check
  IF current_database() NOT IN ('atlas_dev', 'atlas_dev_test', 'nammayatri_dev') THEN
    RAISE NOTICE 'Skipping MSIL dashboard seed: database "%" does not look like a local dev DB.', current_database();
    RETURN;
  END IF;

  -- Skip if MSIL already exists
  IF EXISTS (SELECT 1 FROM atlas_bpp_dashboard.merchant WHERE short_id = 'MSIL_PARTNER') THEN
    RAISE NOTICE 'MSIL_PARTNER dashboard merchant already exists — skipping.';
    RETURN;
  END IF;

  -- BPP Dashboard merchant
  INSERT INTO atlas_bpp_dashboard.merchant (id, short_id, server_name, server_names, created_at, is2fa_mandatory, default_operating_city, supported_operating_cities, domain, enabled, require_admin_approval_for_fleet_onboarding, is_strong_name_check_required, verify_fleet_while_login)
  SELECT md5('MSIL_PARTNER'), 'MSIL_PARTNER', server_name, server_names, now(), is2fa_mandatory, 'Delhi', '{Delhi}', domain, true, false, true, false
  FROM atlas_bpp_dashboard.merchant WHERE short_id = 'NAMMA_YATRI_PARTNER'
  ON CONFLICT DO NOTHING;

  -- Admin user for MSIL (email: admin@msil.test, password: msil1234)
  INSERT INTO atlas_bpp_dashboard.person (id, first_name, last_name, email_encrypted, email_hash,
    mobile_number_encrypted, mobile_number_hash, mobile_country_code, password_hash,
    created_at, updated_at, role_id, dashboard_access_type)
  VALUES (
    'msil-admin-person-0000-000000000000',
    'msil_admin', 'msil_admin',
    '0.1.0|10|O1gV8mZj+pfH2RnjMIPUtvMe6L65I2L2ebX1VTjKUn62ngo3yhPn7wXTbbLuabOA8rNogv2Op/pHTpjejKc7',
    '\x5be49e8d365fe0386b5622639231b0e85daa0c0102eba79ddfd752e128fb9acf',
    '0.1.0|9|1hASG8tb97xmjHFAW/K6M6IstN9roGan6X/q6LxTbQX4dlkLhvKNHmhY2zFmHwr1JEPnGMzdMDFJPQ==',
    '\x2be4774596581441d5228cec7f643b53401baaa343712058ab4939632f771b7f',
    '+91',
    '\x9aafdd537a2c47de0108f549c01ec0d9a8b8c187eb1156c8f830ec8af7f746de',
    now(), now(),
    '37947162-3b5d-4ed6-bcac-08841be1534d',
    'DASHBOARD_ADMIN'
  ) ON CONFLICT DO NOTHING;

  -- Admin merchant_access for MSIL
  INSERT INTO atlas_bpp_dashboard.merchant_access (id, person_id, merchant_id, merchant_short_id, operating_city, created_at, is2fa_enabled)
  VALUES (
    md5('msil-admin-merchant-access'),
    'msil-admin-person-0000-000000000000',
    md5('MSIL_PARTNER'),
    'MSIL_PARTNER',
    'Delhi',
    now(),
    false
  ) ON CONFLICT DO NOTHING;

  -- Admin access matrix for fleet account management
  INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_action_type, user_access_type, created_at, updated_at)
  VALUES
    (md5('msil-admin-fetch-unverified'), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'PROVIDER_MANAGEMENT/ACCOUNT/GET_ACCOUNT_FETCH_UNVERIFIED_ACCOUNTS', 'USER_FULL_ACCESS', now(), now()),
    (md5('msil-admin-verify-account'), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'PROVIDER_MANAGEMENT/ACCOUNT/POST_ACCOUNT_VERIFY_ACCOUNT', 'USER_FULL_ACCESS', now(), now())
  ON CONFLICT DO NOTHING;

  -- Fleet onboarding access matrix
  INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_action_type, user_access_type, created_at, updated_at)
  VALUES
    (md5('fleet-onboard-configs'), 'e5a69a26-d165-455a-a711-33a41e0d4812', 'DSL', 'PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_DOCUMENT_CONFIGS', 'USER_FULL_ACCESS', now(), now()),
    (md5('fleet-onboard-status'), 'e5a69a26-d165-455a-a711-33a41e0d4812', 'DSL', 'PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_REGISTER_STATUS', 'USER_FULL_ACCESS', now(), now()),
    (md5('fleet-onboard-verify'), 'e5a69a26-d165-455a-a711-33a41e0d4812', 'DSL', 'PROVIDER_FLEET/ONBOARDING/POST_ONBOARDING_VERIFY', 'USER_FULL_ACCESS', now(), now()),
    (md5('fleet-onboard-referral'), 'e5a69a26-d165-455a-a711-33a41e0d4812', 'DSL', 'PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_GET_REFERRAL_DETAILS', 'USER_FULL_ACCESS', now(), now()),
    (md5('fleet-register-v2'), 'e5a69a26-d165-455a-a711-33a41e0d4812', 'DSL', 'PROVIDER_FLEET/REGISTRATION_V2/POST_REGISTRATION_V2_REGISTER', 'USER_FULL_ACCESS', now(), now())
  ON CONFLICT DO NOTHING;

  RAISE NOTICE 'MSIL_PARTNER dashboard merchant + access matrix created.';
END;
$$;
