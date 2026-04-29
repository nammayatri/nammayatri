-- INSERT INTO atlas_safety_dashboard.person (id, first_name, last_name,email_encrypted, email_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, password_hash, created_at, updated_at) VALUES
-- 	('3680f4b5-dce4-4d03-aa8c-5405690e87bd', 'police_admin', 'police_admin', '0.1.0|0|LhbMPLXsyXE0tjkVpk2AsylStET+zn3gLufYYvF+mWEGaXojqY71IUsw/gJWIIWzbQTGsY31FlnT3BL8o360B2kngyHgMg9A3Jnj0I4=', '\xef2654345b65cbe5230f3cc47ff26ff73cfd7023e10ac258b4b88bab8221a181', '0.1.0|0|oJOzop+9gdchzwbhz/EyxkSZ7s4z/irFEpsQrsNmSXbKnfe96m+P9xkFqy8/BFU1sGUhgszM1JKsuJNXBQ==', '\x26d21f3ddcce96b1fab220d6aea0b5341d4653e812d4e18d542577acbdeef640', '+91',	'\x8c03a02fbcb46d7f7624063574892f64f19b9871138edfcfeb4f0361362e567f', '2022-09-06 11:25:42.609155+00', '2022-09-06 11:25:42.609155+00');

-- INSERT INTO atlas_safety_dashboard.registration_token (id, token, person_id, created_at) VALUES
-- 	('b856907d-9fb3-4804-9ae4-a53ca902ea0d', '0f3378e2-da5b-4eac-a0f6-397ca48358de', '3680f4b5-dce4-4d03-aa8c-5405690e87bd', now ());

-- ────────────────────────────────────────────────────────────
-- moved from dev/feature-migrations/0017-safety-dashboard-bootstrap-seed.sql
-- ────────────────────────────────────────────────────────────
-- WARNING: LOCAL/DEV BOOTSTRAP SEED — extracted from dev/ddl-migrations/safety-dashboard/
-- These rows were originally inlined inside the schema migrations under
-- dev/ddl-migrations/safety-dashboard/. They were extracted here so the migrations
-- directory contains only DDL. Apply this AFTER the matching DDL migrations.

-- ── from 0000-safety-dashboard.sql ─────────────────────────────────

-- INSERT INTO atlas_safety_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at) VALUES
--     ('37947162-3b5d-4ed6-bcac-08841be1534d', 'POLICE_ADMIN', 'DASHBOARD_ADMIN', 'Police admin can create and assign other roles', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
--     ('e5a69a26-d165-455a-a711-33a41e0d47c6', 'MERCHANT_ADMIN', 'MERCHANT_ADMIN', 'Merchant admin can create and assign roles at partner level', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
--     ('84a47cba-3e8b-4205-aba8-379eaaa28c78', 'MERCHANT_MAKER', 'MERCHANT_MAKER', 'Merchant admin can create and assign roles at partner level', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00');

-- UPDATE atlas_safety_dashboard.person
--     SET role_id = '37947162-3b5d-4ed6-bcac-08841be1534d' where first_name = 'police_admin';

-- INSERT INTO atlas_safety_dashboard.merchant (id, short_id, server_name,default_operating_city,supported_operating_cities, created_at) VALUES
--     ('94bbea0d-3c52-479b-81f5-eca4969ae797', 'NAMMA_YATRI_PARTNER', 'DRIVER_OFFER_BPP','Bangalore',ARRAY['Bangalore'], now ());

-- INSERT INTO atlas_safety_dashboard.merchant_access (id, person_id, created_at, merchant_id, is2fa_enabled, merchant_short_id, operating_city) VALUES
--  ('a1418ee5-acc1-f83f-0a63-1890349494b0', '3680f4b5-dce4-4d03-aa8c-5405690e87bd', '2022-09-12 15:15:42.104639+00', '94bbea0d-3c52-479b-81f5-eca4969ae797', false, 'NAMMA_YATRI_PARTNER', 'Bangalore');

-- UPDATE atlas_safety_dashboard.registration_token
--     SET merchant_id = '94bbea0d-3c52-479b-81f5-eca4969ae797' WHERE server_name = 'DRIVER_OFFER_BPP';

-- UPDATE atlas_safety_dashboard.merchant as T1
-- SET server_names = ARRAY[server_name];

-- UPDATE atlas_safety_dashboard.merchant set server_names = '{DRIVER_OFFER_BPP, DRIVER_OFFER_BPP_MANAGEMENT}' where server_name = 'DRIVER_OFFER_BPP';

-- INSERT INTO atlas_safety_dashboard.system_configs (id, config_value)
-- VALUES ('kv_configs', '{"enableKVForWriteAlso":[{"nameOfTable":"Table1","percentEnable":100}],"enableKVForRead":["Table2"]}');

-- INSERT INTO atlas_safety_dashboard.portal_configs
--   (config_name, created_at, id, updated_at, value)
-- VALUES
--   ('BULK_UPLOAD_COUNT', now(), 'a9c80141-6ee8-48a6-a589-e595e5b8a54f',now() , '100');

-- INSERT INTO atlas_safety_dashboard.portal_configs
--   (config_name, created_at, id, updated_at, value)
-- VALUES
--   ('BULK_SEARCH_COUNT', now(), '120f6a56-b88c-4065-8402-d8d5d58ffa07',now() , '100');
-- -- ── from 0005-add-totp-config-to-merchant.sql ─────────────────────────────────

-- -- Backfill sensible defaults (30s step, TwoSteps = ±60s skew) matching cryptonite's defaultTOTPParams.
-- UPDATE atlas_safety_dashboard.merchant SET totp_step_size = 30 WHERE totp_step_size IS NULL;
-- UPDATE atlas_safety_dashboard.merchant SET totp_clock_skew = 2 WHERE totp_clock_skew IS NULL;
-- -- ── Restore DEFAULT + NOT NULL on FK columns (originally inlined in DDL ADD COLUMN) ─────
-- UPDATE atlas_safety_dashboard.person
--    SET role_id = 'e5a69a26-d165-455a-a711-33a41e0d47c6'
--  WHERE role_id IS NULL;
-- ALTER TABLE atlas_safety_dashboard.person
--    ALTER COLUMN role_id SET DEFAULT 'e5a69a26-d165-455a-a711-33a41e0d47c6';
-- ALTER TABLE atlas_safety_dashboard.person
--    ALTER COLUMN role_id SET NOT NULL;

-- UPDATE atlas_safety_dashboard.registration_token
--    SET merchant_id = '94bbea0d-3c52-479b-81f5-eca4969ae797'
--  WHERE merchant_id IS NULL;
-- ALTER TABLE atlas_safety_dashboard.registration_token
--    ALTER COLUMN merchant_id SET DEFAULT '94bbea0d-3c52-479b-81f5-eca4969ae797';
-- ALTER TABLE atlas_safety_dashboard.registration_token
--    ALTER COLUMN merchant_id SET NOT NULL;
