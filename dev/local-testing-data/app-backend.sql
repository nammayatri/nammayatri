-- Phone number: 9999999999
INSERT INTO atlas_app.person(id, role, gender, identifier_type, email, password_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, verified, status, created_at, updated_at) values
  ('ec34eede-5a3e-4a41-89d4-7290a0d7a629', 'USER', 'UNKNOWN', 'MOBILENUMBER', NULL, NULL, '0.1.0|1|8hdNzNRjeq0j7QRZoKzT2iMjoKkQJzwLoUeOEF9edh1wmRer7NbUaUT3foUI1wG2gZMjpB8vDuUKUDG+aQ==', '\xba0f72d2d550eb699914591dab2eadeb8fc83dd1eda6bde7cc3f68d7138f14fe', '+91', NULL, false, 'INACTIVE', now(), now());

-- Password: Support@123
INSERT INTO atlas_app.person(id, role, gender, identifier_type, email, password_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, verified, status, created_at, updated_at) values
  ('5628baed-d1cb-41e0-a731-601816c4ac5a', 'CUSTOMER_SUPPORT', 'UNKNOWN', 'EMAIL', 'support@local', '\x6f52c5d850cdb02b85ee3e5ab03466d6b367d3d3fbbabe62ed5005f82d2a0fc9', NULL, NULL, NULL, 'supportuserid', false, 'ACTIVE', now(), now());

INSERT INTO atlas_app.registration_token (id, auth_medium, auth_type, auth_value_hash, token, verified, auth_expiry, token_expiry, attempts, entity_id, entity_type, created_at, updated_at) values
  ('772453e2-d02b-494a-a4ac-ec1ea0027e18', 'SMS', 'OTP', '3249', 'ea37f941-427a-4085-a7d0-96240f166672', false, 3, 365, 3, 'ec34eede-5a3e-4a41-89d4-7290a0d7a629', 'USER', now(), now());

-- User

INSERT INTO atlas_app.location (id, location_type, lat, long, point) VALUES
  ('003d2f36-a455-4625-bfb4-22807fefa1eb', 'POINT', 76.2733, 10, public.ST_SetSRID(public.ST_Point(76.2733, 10), 4326));

-- Phone number: 7777777777
INSERT INTO atlas_app.person (id, first_name, middle_name, last_name, full_name, role, gender, identifier_type, email, password_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, rating, verified, udf1, udf2, status, organization_id, device_token, location_id, description, created_at, updated_at) VALUES
  ('003b93df-4f7c-440f-bada-4d46c396d7d0', 'Some', 'Cool', 'User', NULL, 'USER', 'FEMALE', 'MOBILENUMBER', NULL, NULL,  '0.1.0|0|XiWzE3GGzRJQIjsQPCQO/X+Nj5Srv1mPM463Q2QlvXeWjo8hzAlJ6wfxYtpDvZr3bzONXitCP57nOH7FWQ==', '\x1604f4cfb781d8f1c9ff024f8d06abc265a2d6954ac59079ea0c11f9f0bc1fa4', '+91', NULL, NULL, true, NULL, NULL, 'ACTIVE', NULL, NULL, '003d2f36-a455-4625-bfb4-22807fefa1eb', NULL, now(), now());

INSERT INTO atlas_app.registration_token (id, auth_medium, auth_type, auth_value_hash, token, verified, auth_expiry, token_expiry, attempts, entity_id, entity_type, created_at, updated_at) values
  ('003d53e2-d02b-494a-a4ac-ec1ea0027e18', 'SMS', 'OTP', '4321', '003df941-427a-4085-a7d0-96240f166672', true, 3, 365, 3, '003b93df-4f7c-440f-bada-4d46c396d7d0', 'USER', now(), now());
