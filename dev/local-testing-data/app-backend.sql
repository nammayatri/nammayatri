-- Phone number: 9999999999
INSERT INTO atlas_app.person(id, role, gender, identifier_type, email, password_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, verified, status, created_at, updated_at) values
  ('ec34eede-5a3e-4a41-89d4-7290a0d7a629', 'USER', 'UNKNOWN', 'MOBILENUMBER', NULL, NULL, '0.1.0|1|8hdNzNRjeq0j7QRZoKzT2iMjoKkQJzwLoUeOEF9edh1wmRer7NbUaUT3foUI1wG2gZMjpB8vDuUKUDG+aQ==', '\xba0f72d2d550eb699914591dab2eadeb8fc83dd1eda6bde7cc3f68d7138f14fe', '+91', NULL, false, 'INACTIVE', now(), now());

-- Password: Support@123
INSERT INTO atlas_app.person(id, role, gender, identifier_type, email, password_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, verified, status, created_at, updated_at) values
  ('5628baed-d1cb-41e0-a731-601816c4ac5a', 'CUSTOMER_SUPPORT', 'UNKNOWN', 'EMAIL', 'support@local', '\x6f52c5d850cdb02b85ee3e5ab03466d6b367d3d3fbbabe62ed5005f82d2a0fc9', NULL, NULL, NULL, 'supportuserid', false, 'ACTIVE', now(), now());

INSERT INTO atlas_app.registration_token (id, auth_medium, auth_type, auth_value_hash, token, verified, auth_expiry, token_expiry, attempts, entity_id, entity_type, created_at, updated_at) values
  ('772453e2-d02b-494a-a4ac-ec1ea0027e18', 'SMS', 'OTP', '3249', 'ea37f941-427a-4085-a7d0-96240f166672', false, 3, 365, 3, 'ec34eede-5a3e-4a41-89d4-7290a0d7a629', 'USER', now(), now());

-- Transporter entries
-- callback_url - is used to select transporter at BPP end
-- callback_api_key - will be same for all transporters since this corresponds to one BAP entry at BPP.
-- api_key - All transporters will send callback with the same api_key, but we dont use the matched org in the callback flow.
--           Since api_key field is unique, adding dummy values to subsequent trasporter entries.
INSERT INTO atlas_app.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, callback_api_key, head_count, info, created_at, updated_at) VALUES
  ('70c76e36-f035-46fd-98a7-572dc8934323', '[A] Transporter #1', 'JUSPAY.MOBILITY.PROVIDER.UAT.1', NULL, 'APPROVED', 'PROVIDER', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'bap-key-1', 'http://localhost:8014/v1/7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'bpp-1-key', NULL, NULL, now(), now());
INSERT INTO atlas_app.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, callback_api_key, head_count, info, created_at, updated_at) VALUES
  ('1257a139-6039-40b8-8752-96a77311f645', '[A] Transporter #2', 'another-test-cabs', NULL, 'APPROVED', 'PROVIDER', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'bap-key-2', 'http://localhost:8014/v1/e1f37274-f0aa-4bb3-93a0-2476349487b7', 'bpp-1-key', NULL, NULL, now(), now());

INSERT INTO atlas_app.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, callback_api_key, head_count, info, created_at, updated_at) VALUES
  ('e12d9178-4e88-497e-a310-482b6664dc06', 'Juspay Gateway', 'JUSPAY.BG.1', NULL, 'APPROVED', 'GATEWAY', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'mobility-gateway-key', 'http://localhost:8015/v1', 'mobility-app-key', NULL, NULL, now(), now());

INSERT INTO atlas_app.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, callback_api_key, head_count, info, created_at, updated_at) VALUES
  ('239ee68b-0818-4dba-ad31-032fe809cf71', 'NSDL Gateway', 'NSDL.BG.1', NULL, 'APPROVED', 'GATEWAY', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'nsdl-gateway-key', 'https://gateway-1.beckn.nsdl.co.in/v1', 'nsdl-app-key', NULL, NULL, now(), now());

-- User

INSERT INTO atlas_app.location (id, location_type, lat, long, point) VALUES
  ('003d2f36-a455-4625-bfb4-22807fefa1eb', 'POINT', 76.2733, 10, public.ST_SetSRID(public.ST_Point(76.2733, 10), 4326));

-- Phone number: 7777777777
INSERT INTO atlas_app.person (id, first_name, middle_name, last_name, full_name, role, gender, identifier_type, email, password_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, rating, verified, udf1, udf2, status, organization_id, device_token, location_id, description, created_at, updated_at) VALUES
  ('003b93df-4f7c-440f-bada-4d46c396d7d0', 'Some', 'Cool', 'User', NULL, 'USER', 'FEMALE', 'MOBILENUMBER', NULL, NULL,  '0.1.0|0|XiWzE3GGzRJQIjsQPCQO/X+Nj5Srv1mPM463Q2QlvXeWjo8hzAlJ6wfxYtpDvZr3bzONXitCP57nOH7FWQ==', '\x1604f4cfb781d8f1c9ff024f8d06abc265a2d6954ac59079ea0c11f9f0bc1fa4', '+91', NULL, NULL, true, NULL, NULL, 'ACTIVE', NULL, NULL, '003d2f36-a455-4625-bfb4-22807fefa1eb', NULL, now(), now());

INSERT INTO atlas_app.registration_token (id, auth_medium, auth_type, auth_value_hash, token, verified, auth_expiry, token_expiry, attempts, entity_id, entity_type, created_at, updated_at) values
  ('003d53e2-d02b-494a-a4ac-ec1ea0027e18', 'SMS', 'OTP', '4321', '003df941-427a-4085-a7d0-96240f166672', true, 3, 365, 3, '003b93df-4f7c-440f-bada-4d46c396d7d0', 'USER', now(), now());


INSERT INTO atlas_app.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('metro-0a-81d2-404b-ada5-20aac58005e6', 'Metro BPP', 'metro-bpp', NULL, 'APPROVED', 'PROVIDER', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'key1', 'http://4c75-49-207-207-115.ngrok.io', NULL, now(), now(), 'key1', NULL);
