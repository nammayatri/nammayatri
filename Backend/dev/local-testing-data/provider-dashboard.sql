-- customer: +91 7777777777; driver: +91 7777777778; juspay_ops: +91 7777777779; juspay_admin: +91 7777777780; customer_service: +91 7777777781; fleet_owner: +92 6666666666; rental_fleet_owner: +91 6666666667; operator: +91 7777777782;
-- role values resolved against atlas_bpp_dashboard.role rows seeded by
-- dev/seed-migrations/provider-dashboard/0001-roles.sql:
--   CUSTOMER           = e5a69a26-d165-455a-a711-33a41e0d47c6
--   DRIVER             = 508a0bac-258d-44a6-ac55-aef57ab87a76
--   JUSPAY_OPS         = d5644e83-ffa3-4e0d-ae81-c3155eedb8fd
--   JUSPAY_ADMIN       = 37947162-3b5d-4ed6-bcac-08841be1534d
--   CUSTOMER_SERVICE   = a708c6a1-78b5-4e5e-9df8-468cd81dc2aa
--   FLEET              = e5a69a26-d165-455a-a711-33a41e0d4812
--   RENTAL_FLEET_OWNER = 00000000-0000-0000-rental-fleet-role
--   OPERATOR           = 00000000-0000-0000-000-operator-role
INSERT INTO atlas_bpp_dashboard.person (id, first_name, last_name, role_id, email_encrypted, email_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, password_hash, created_at, updated_at) VALUES
	('25e77f37-75e5-4665-8ed0-4be2af35940a', 'customer', 'customer', 'e5a69a26-d165-455a-a711-33a41e0d47c6', '0.1.0|0|/UkZKAXB/0WX+43O5fkXmKnuMzvJFUKwiI57hVKqpg9CMWCQ45wjXU/4JX/+I5xajr1FZvdc8WPPGbmFzOr7gknP63NC9wuXOw==', '\xe7aba02fcf4082bb36c9ea9f3b41e54de1307176ababd76e9059988f0aa57214', '0.1.0|0|j4VGXmuvt7HU5qP+En30NqGAn2z8KpRGFeJVd8H/VAWPB3mliuNjeQWteRoXUC7yYOOMjV6m0uYkJqy/Ww==', '\x1604f4cfb781d8f1c9ff024f8d06abc265a2d6954ac59079ea0c11f9f0bc1fa4', '+91', '\xea29f9013ed2a5b6da7f20b3c3b3f429bd8f55413dc35fc98403d017edc6cd1d', '2022-09-06 11:25:45.564321+00', '2022-09-06 11:25:45.564321+00'),
	('cd69ae25-1641-4e6d-b9f4-5bae63e2a537', 'driver', 'driver', '508a0bac-258d-44a6-ac55-aef57ab87a76', '0.1.0|2|RW7L3VAgJptc2fyG6mEn1P6rUxYagnT9AC5bhnKBmBkEVlR9IhsXLOUuu0F4SvUWozkQYouvMgARNL6x/tlVRm/YA6udkmU=', '\x7e25a5f3b66b86b00d1e2c7c3aacc9de05f5b64ca6e953bd825d861fa0206ee2', '0.1.0|0|O4pIfqluEqP2UWbMcSDj26BCt5eKapJDHhvFajnrwFKYC7ux8MmPXLUqEkA3pfHz5dOu4LgF/qrHdMklKA==', '\x017cc744055f27a166b08a99b08adff09d77cd2fd36f9017b17c749bbaff3fb8', '+91', '\x1b2c79ff2ba0b4eacb1fff0944b89be973f77c43a989abcc3fbf2762003d1cf9', '2022-09-06 11:25:44.591696+00', '2022-09-06 11:25:44.591696+00'),
	('a77b0507-ae01-42dd-a075-264f59d89049', 'juspay_ops', 'juspay_ops', 'd5644e83-ffa3-4e0d-ae81-c3155eedb8fd', '0.1.0|1|2Yjtb/THzo0gn3Nrlv/nUD5loCHgZ+sF8qdo1tLZpmzmUNsTLDKTbW6jDlWCBAl/U3vzYWaOZRTU7QULoq9Rn6bgOaSqiM7Eun2I', '\x2a796a562e683a3dca90cb8e17fe5781ef59d31b318379c292e40314e480f3cd', '0.1.0|0|6UvxFEKkKoAcTNBTUIsioH1eS7oqmlf8M6D21qTqyzfUtZBdx9pLgu6Id0unL08M4zVWYVyJXby//SGtRQ==', '\xd9a0478bcb0f95155df43d639bb5d368118d3d74f804e7c16f89679abf274c14', '+91', '\x1610d494c1be63c14a1dfb6a5797c5e16dcbfcc836d9c073863ed4e27ee4c4bb', '2022-09-06 11:25:43.601821+00', '2022-09-06 11:25:43.601821+00'),
	('3680f4b5-dce4-4d03-aa8c-5405690e87bd', 'juspay_admin', 'juspay_admin', '37947162-3b5d-4ed6-bcac-08841be1534d', '0.1.0|0|LhbMPLXsyXE0tjkVpk2AsylStET+zn3gLufYYvF+mWEGaXojqY71IUsw/gJWIIWzbQTGsY31FlnT3BL8o360B2kngyHgMg9A3Jnj0I4=', '\xef2654345b65cbe5230f3cc47ff26ff73cfd7023e10ac258b4b88bab8221a181', '0.1.0|0|oJOzop+9gdchzwbhz/EyxkSZ7s4z/irFEpsQrsNmSXbKnfe96m+P9xkFqy8/BFU1sGUhgszM1JKsuJNXBQ==', '\x26d21f3ddcce96b1fab220d6aea0b5341d4653e812d4e18d542577acbdeef640', '+91',	'\x8c03a02fbcb46d7f7624063574892f64f19b9871138edfcfeb4f0361362e567f', '2022-09-06 11:25:42.609155+00', '2022-09-06 11:25:42.609155+00'),
	('59f5bd8c-8268-4e8e-bcab-c21da7e496d4', 'customer_service', 'customer_service', 'a708c6a1-78b5-4e5e-9df8-468cd81dc2aa', '0.1.0|1|9KSgxavNrMrth8w2zBzxhW0dA+yTK+Pub7awBH//7Fzu8fnZn3OWWGW0sLu6AKHtg9DdAD8bVG9r0n10P5d/507Aq8A6M7bEwMMzTJwCuXwB', '\x665ddab8860069681b2a5727b8983315408a39a6f3064734bd146236bd825438', '0.1.0|2|JaxIBt2gn9eHBYnoDpSF3hJm+KtfXNHtzaR9LZVEZUHnM6lxscEfLdGMj+5e9suZwVA0sMlKFUhr/7JmMw==', '\xbb63874220d75c23f6b49a63e2c0232408d0dc49c0e1c1bd3c004400488f0db5', '+91', '\x7aa08a0e89a92bd13ce544fa7bd58fbada3862919306a072240df98470e42792', '2022-09-06 11:25:41.633041+00', '2022-09-06 11:25:41.633041+00'),
	('favorit-fleet-owner-0000000000000000', 'fleet_owner', 'fleet_owner', 'e5a69a26-d165-455a-a711-33a41e0d4812', '0.1.0|1|/QZ+NzV6v2DkGqf6LXpH+x82JkPBJvvWPDeGb37zIHy6rVp4ZsLef71pYdxdviVo5wI3+S0ORMg+V8VgrBWpWHZICyCaG0bXrQdR+A==', '\xef95936969937e991c387ea57d4eadbe7559c1c6cba069af3ff12ef2ad6cce2a', '0.1.0|0|iP3CepsEe8Qmw1xbLR5HJFSESfdvU2tWtNWrdCZWtwp4msTfh1BDkc95/yytpllMp61Q8mpiS+KDde+Plw==', '\xa0a56e902b973e6cf231520c2acbda9b44947dd3a88fb0daacd23d68082c6362', '+92',	'\x8c03a02fbcb46d7f7624063574892f64f19b9871138edfcfeb4f0361362e567f', '2022-09-06 11:25:41.633041+00', '2022-09-06 11:25:41.633041+00'),
	('favorit-rental_fleet-owner-000000000', 'rental_fleet_owner', 'rental_fleet_owner', '00000000-0000-0000-rental-fleet-role', '0.1.0|0|RPjEoHz7AWbyRdeawHb7YeaqvxBl2AFzo/cWvcSPE0pC+AQwn1YyGykSCVBIMDtLyrsdhLul/Hi3Wigb4xlia+VkkmnA4UU4hZFqEqPWCwo9fik=', '\xc18c80caba29b9eb3d5bc00c5f872f8a445d6301118d6813ff553a2b0c991a47', '0.1.0|2|grztTGLFW5CMS6Xr3jDPKp4rcpDlJBTIRDumCxXDzBS21kTlll4RhTeTo2lAfRqaizQZMJHx3p9oNhA29w==', '\x2e2699c49e9713f25031974d7978cfcb4781fd626b8ecff4b3bab1f829f0778d', '+91','\x8c03a02fbcb46d7f7624063574892f64f19b9871138edfcfeb4f0361362e567f', '2022-09-06 11:25:41.633041+00', '2022-09-06 11:25:41.633041+00'),
	('favorit-operator-0-0000-000000000000', 'operator', 'operator', '00000000-0000-0000-000-operator-role', '0.1.0|1|WHgN2k1Fddh8M1g5BaetWRX289my+lCeuGRUXN90ECxF1q/DLi+LHhwIzA7OFaGcO4A3UfIOg+IaQdNxv0Y9sjmmBR14XksM6Q==', '\x8e90c91d09f382d419627e3dcbf3ccd00edd967a71a6271afae890c15d46f1ce', '0.1.0|0|11QzDhTjUgXJPxhNyM5x2Ovk+20gkQelfugVx85sEWsQXSeJ1Pj6uAjhYwqDONST0obPt2vGZ5j/eUhEBA==', '\x04a9ed6bfa58db4576ee9ff5a34549ac1176755d4840ce4d38e1c16efa75ae2a', '+91',	'\x8c03a02fbcb46d7f7624063574892f64f19b9871138edfcfeb4f0361362e567f', '2022-09-06 11:25:41.633041+00', '2022-09-06 11:25:41.633041+00');

INSERT INTO atlas_bpp_dashboard.registration_token (id, token, person_id, operating_city, created_at) VALUES
	('50666614-c6f1-48a8-ab16-23873b93f492', '512e0a7b-e521-4d6b-a0c4-8713ae345bf2', '25e77f37-75e5-4665-8ed0-4be2af35940a', 'Bangalore', now ()),
	('57b6a4ed-6f74-4260-93eb-5c985fd5d776', '223ed551-c9f8-4bae-b49a-30a696424b7f', 'cd69ae25-1641-4e6d-b9f4-5bae63e2a537', 'Bangalore', now ()),
	('a7f18c42-43bf-4ab9-9fdd-846efa83f2dc', 'e789b47f-2e39-47a2-b297-33f8fd4b87d0', 'a77b0507-ae01-42dd-a075-264f59d89049', 'Bangalore', now ()),
	('b856907d-9fb3-4804-9ae4-a53ca902ea0d', '0f3378e2-da5b-4eac-a0f6-397ca48358de', '3680f4b5-dce4-4d03-aa8c-5405690e87bd', 'Bangalore', now ()),
	('c9c0375f-5885-4dab-acc9-27552c31db90', '07ef2a2a-b014-4a86-b1c0-e453e8b0b66e', '59f5bd8c-8268-4e8e-bcab-c21da7e496d4', 'Bangalore', now ()),
	('88c0375f-5885-4dab-acc9-27552c31db88', '88ef2a2a-b014-4a86-b1c0-e453e8b0b688', 'favorit-fleet-owner-0000000000000000', 'Bangalore', now ()),
	('favorit-rental-fleet-token-id-000000', 'favorit-rental_fleet-token-000000000', 'favorit-rental_fleet-owner-000000000', 'Bangalore', now ()),
	('favorit-operator-token-id-0000000000', 'favorit-operator-token0-000000000000', 'favorit-operator-0-0000-000000000000', 'Bangalore', now ());

-- ── Admin merchant_access for the test admin person, per merchant × city ──
-- atlas_bpp_dashboard.merchant_operating_city / merchant come from config-sync.
-- We only seed merchant_access rows so the local test admin can switch
-- merchants/cities in the dashboard.

DO $$
DECLARE
    admin_person_id TEXT := '3680f4b5-dce4-4d03-aa8c-5405690e87bd';
    r RECORD;
    city_name TEXT;
BEGIN
    FOR r IN SELECT id, short_id FROM atlas_bpp_dashboard.merchant
    LOOP
        FOREACH city_name IN ARRAY ARRAY['Bangalore','Helsinki','Kolkata','Chennai','Delhi']
        LOOP
            INSERT INTO atlas_bpp_dashboard.merchant_access (id, person_id, merchant_id, merchant_short_id, operating_city, secret_key, is2fa_enabled, created_at)
            SELECT gen_random_uuid()::text, admin_person_id, r.id, r.short_id, city_name, '', false, now()
            WHERE NOT EXISTS (
                SELECT 1 FROM atlas_bpp_dashboard.merchant_access
                WHERE person_id = admin_person_id AND operating_city = city_name AND merchant_id = r.id
            );
        END LOOP;
    END LOOP;
END $$;

-- ONLY FOR LOCAL
UPDATE atlas_bpp_dashboard.person SET dashboard_access_type = 'DASHBOARD_OPERATOR'
    WHERE id = 'favorit-operator-0-0000-000000000000';
UPDATE atlas_bpp_dashboard.person SET dashboard_access_type = 'FLEET_OWNER'
    WHERE id = 'favorit-fleet-owner-0000000000000000';
UPDATE atlas_bpp_dashboard.person SET dashboard_access_type = 'RENTAL_FLEET_OWNER'
    WHERE id = 'favorit-rental_fleet-owner-000000000';
-- ── from 0062-add-require-approval-in-merchant.sql ─────────────────────────────────

UPDATE atlas_bpp_dashboard.merchant
SET require_admin_approval_for_fleet_onboarding = true,
    has_fleet_member_hierarchy = false
WHERE short_id = 'MSIL_PARTNER_LOCAL';

UPDATE atlas_bpp_dashboard.merchant
SET verify_fleet_while_login = false
WHERE short_id = 'MSIL_PARTNER_LOCAL';

UPDATE atlas_bpp_dashboard.merchant
SET require_admin_approval_for_fleet_onboarding = false
WHERE short_id = 'MSIL_PARTNER_LOCAL';
-- ── from 0077-registration-token-dashboard.sql ─────────────────────────────────
-- Admin token for Bangalore with correct merchant_id and operating_city
INSERT INTO atlas_bpp_dashboard.registration_token (id, token, person_id, merchant_id, operating_city, enabled, created_at)
VALUES (
    'local-admin-token-blr-id-00000000000',
    'local-admin-token-bangalore-namma-yatri',
    '3680f4b5-dce4-4d03-aa8c-5405690e87bd',
    '94bbea0d-3c52-479b-81f5-eca4969ae797',
    'Bangalore',
    true,
    now()
);
-- ── Restore DEFAULT + NOT NULL on FK columns (originally inlined in DDL ADD COLUMN) ─────
UPDATE atlas_bpp_dashboard.person
   SET role_id = 'e5a69a26-d165-455a-a711-33a41e0d47c6'
 WHERE role_id IS NULL;
ALTER TABLE atlas_bpp_dashboard.person
   ALTER COLUMN role_id SET DEFAULT 'e5a69a26-d165-455a-a711-33a41e0d47c6';
ALTER TABLE atlas_bpp_dashboard.person
   ALTER COLUMN role_id SET NOT NULL;

UPDATE atlas_bpp_dashboard.merchant_access
   SET merchant_id = 'd92db186-39d3-48a4-ad1f-78a0c3f840fd'
 WHERE merchant_id IS NULL;
ALTER TABLE atlas_bpp_dashboard.merchant_access
   ALTER COLUMN merchant_id SET DEFAULT 'd92db186-39d3-48a4-ad1f-78a0c3f840fd';
ALTER TABLE atlas_bpp_dashboard.merchant_access
   ALTER COLUMN merchant_id SET NOT NULL;

UPDATE atlas_bpp_dashboard.registration_token
   SET merchant_id = 'd92db186-39d3-48a4-ad1f-78a0c3f840fd'
 WHERE merchant_id IS NULL;
ALTER TABLE atlas_bpp_dashboard.registration_token
   ALTER COLUMN merchant_id SET DEFAULT 'd92db186-39d3-48a4-ad1f-78a0c3f840fd';
ALTER TABLE atlas_bpp_dashboard.registration_token
   ALTER COLUMN merchant_id SET NOT NULL;

-- ────────────────────────────────────────────────────────────
-- moved from dev/feature-migrations/0018-test-dashboard-msil-provider-dashboard-seed.sql
-- ────────────────────────────────────────────────────────────
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
  IF EXISTS (SELECT 1 FROM atlas_bpp_dashboard.merchant WHERE short_id = 'MSIL_PARTNER_LOCAL') THEN
    RAISE NOTICE 'MSIL_PARTNER dashboard merchant already exists — skipping.';
    RETURN;
  END IF;

  -- BPP Dashboard merchant
  INSERT INTO atlas_bpp_dashboard.merchant (id, short_id, server_name, server_names, created_at, is2fa_mandatory, default_operating_city, supported_operating_cities, domain, enabled, require_admin_approval_for_fleet_onboarding, is_strong_name_check_required, verify_fleet_while_login)
  SELECT md5('MSIL_PARTNER_LOCAL'), 'MSIL_PARTNER_LOCAL', server_name, server_names, now(), is2fa_mandatory, 'Delhi', '{Delhi}', domain, true, false, true, false
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
    md5('MSIL_PARTNER_LOCAL'),
    'MSIL_PARTNER_LOCAL',
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

-- ────────────────────────────────────────────────────────────
-- moved from dev/feature-migrations/0019-test-dashboard-lynx-provider-dashboard-seed.sql
-- ────────────────────────────────────────────────────────────
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
  IF EXISTS (SELECT 1 FROM atlas_bpp_dashboard.merchant WHERE short_id = 'LYNX_PARTNER_LOCAL') THEN
    RAISE NOTICE 'LYNX_PARTNER dashboard merchant already exists — skipping.';
    RETURN;
  END IF;
-- LOCAL FIX DO NOT RUN IN PROD ? MASTER
  -- BPP Dashboard merchant — admin approval required for fleet onboarding
  INSERT INTO atlas_bpp_dashboard.merchant (id, short_id, server_name, server_names, created_at, is2fa_mandatory, default_operating_city, supported_operating_cities, domain, enabled, require_admin_approval_for_fleet_onboarding, is_strong_name_check_required, verify_fleet_while_login)
  SELECT md5('LYNX_PARTNER_LOCAL'), 'LYNX_PARTNER_LOCAL', server_name, server_names, now(), is2fa_mandatory, 'Helsinki', '{Helsinki}', domain, true, true, false, false
  FROM atlas_bpp_dashboard.merchant WHERE short_id = 'NAMMA_YATRI_PARTNER'
  ON CONFLICT DO NOTHING;

  v_lynx_dashboard_merchant_id := md5('LYNX_PARTNER_LOCAL');

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
    'LYNX_PARTNER_LOCAL',
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
