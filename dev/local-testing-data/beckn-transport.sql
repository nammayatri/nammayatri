--
-- Data for Name: organization; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

-- Gateway entries
INSERT INTO atlas_transporter.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, callback_api_key, head_count, created_at, updated_at, info) VALUES
  ('1926d40f-1223-4eb2-ba5d-7983bde2fd02', 'Juspay Gateway', 'JUSPAY.BG.1', NULL, 'APPROVED', 'GATEWAY', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'http://localhost:8015/v1', NULL, NULL, now(), now(), NULL);
-- NSDL creds are fetched from config
INSERT INTO atlas_transporter.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, callback_api_key, head_count, created_at, updated_at, info) VALUES
  ('0d7016d0-f9cd-4f9f-886f-bc4cbd6a86e5', 'NSDL Gateway', 'NSDL', NULL, 'APPROVED', 'GATEWAY', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'https://gateway-1.beckn.nsdl.co.in/v1', NULL, NULL, now(), now(), NULL);

-- Transporter entries. api_key, callback_url, callback_api_key are not needed to be populated. These are only looked up from id.
INSERT INTO atlas_transporter.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, callback_api_key, head_count, created_at, updated_at, info) VALUES
  ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', '[A] Transporter #1', 'JUSPAY.MOBILITY.PROVIDER.UAT.1', NULL, 'APPROVED', 'PROVIDER', 'MOBILITY', false, true, 'e95d2f36-a455-4625-bfb4-22807fefa1eb', NULL, '9888888888', '+91', NULL, NULL, NULL, NULL, NULL, NULL, now(), now(), NULL);
INSERT INTO atlas_transporter.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, callback_api_key, head_count, created_at, updated_at, info) VALUES
  ('e1f37274-f0aa-4bb3-93a0-2476349487b7', 'Another Test Cabs', 'another-test-cabs', NULL, 'APPROVED', 'PROVIDER', 'MOBILITY', true, true, 'e95d2f36-a455-4625-bfb4-22807fefa1eb', NULL, '9111111111', '+91', NULL, NULL, NULL, NULL, NULL, NULL, now(), now(), NULL);

-- BAP app entry.
INSERT INTO atlas_transporter.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, callback_api_key, head_count, created_at, updated_at, info) VALUES
  ('505e4651-5340-4836-81a7-045394ba6dc3', 'Mobility BAP', 'JUSPAY.MOBILITY.APP.UAT.1', NULL, 'APPROVED', 'APP', 'MOBILITY', true, true, NULL, NULL, '9777777777', '+91', NULL, NULL, NULL, 'http://localhost:8013/cab/v1', NULL, NULL, now(), now(), NULL);

-- NSDL entry
INSERT INTO atlas_transporter.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, callback_api_key, head_count, info, created_at, updated_at) VALUES
  ('239ee68b-0818-4dba-ad31-032fe809cf71', 'NSDL Gateway', 'NSDL.BG.1', NULL, 'APPROVED', 'GATEWAY', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'https://gateway-1.beckn.nsdl.co.in/v1', NULL, NULL, NULL, now(), now());

--
-- Data for Name: person; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

INSERT INTO atlas_transporter.person (id, first_name, middle_name, last_name, full_name, role, gender, identifier_type, email, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, rating, verified, udf1, udf2, status, organization_id, device_token, location_id, description, created_at, updated_at) VALUES
  ('6bc4bc84-2c43-425d-8853-22f47driver1', 'Suresh', 'aka', 'Dhinesh', NULL, 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, '0.1.0|0|iP3CepsEe8Qmw1xbLR5HJFSESfdvU2tWtNWrdCZWtwp4msTfh1BDkc95/yytpllMp61Q8mpiS+KDde+Plw==', '\xa0a56e902b973e6cf231520c2acbda9b44947dd3a88fb0daacd23d68082c6362', '+91', NULL, NULL, false, '0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE', 'INACTIVE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, '3b2af603-c6a3-404e-9859-27608driver1', NULL, now(), now());
INSERT INTO atlas_transporter.person (id, first_name, middle_name, last_name, full_name, role, gender, identifier_type, email, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, rating, verified, udf1, udf2, status, organization_id, device_token, location_id, description, created_at, updated_at) VALUES
  ('6bc4bc84-2c43-425d-8853-22f47driver2', 'Bob', 'aka', 'Dhinesh', NULL, 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, '0.1.0|0|iP3CepsEe8Qmw1xbLR5HJFSESfdvU2tWtNWrdCZWtwp4msTfh1BDkc95/yytpllMp61Q8mpiS+KDde+Plw==', '\xa0a56e902b973e6cf231520c2acbda9b44947dd3a88fb0daacd23d68082c6362', '+94', NULL, NULL, false, '0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE', 'INACTIVE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, '3b2af603-c6a3-404e-9859-27608driver2', NULL, now(), now());
INSERT INTO atlas_transporter.person (id, first_name, middle_name, last_name, full_name, role, gender, identifier_type, email, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, rating, verified, udf1, udf2, status, organization_id, device_token, location_id, description, created_at, updated_at) VALUES
  ('3b2af603-c6a3-404e-9859-276085fc6e65', 'Thomas', 'aka', NULL, NULL, 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, NULL, NULL, '+91', NULL, NULL, false, 'f1b84838-5cb5-4eb1-8f4b-0dd0c4a2-suv', 'VEHICLE', 'INACTIVE', 'e1f37274-f0aa-4bb3-93a0-2476349487b7', NULL, '0202b94d-dbf0-4e95-bbf9-25cafc888173', NULL, now(), now());

-- Phone number: 9999999999
INSERT INTO atlas_transporter.person (id, first_name, middle_name, last_name, full_name, role, gender, identifier_type, email, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, rating, verified, udf1, udf2, status, organization_id, device_token, location_id, description, created_at, updated_at) VALUES
  ('ec34eede-5a3e-4a41-89d4-7290a0d7a629', NULL, NULL, NULL, NULL, 'ADMIN', 'UNKNOWN', 'MOBILENUMBER', NULL, '0.1.0|1|8hdNzNRjeq0j7QRZoKzT2iMjoKkQJzwLoUeOEF9edh1wmRer7NbUaUT3foUI1wG2gZMjpB8vDuUKUDG+aQ==', '\xba0f72d2d550eb699914591dab2eadeb8fc83dd1eda6bde7cc3f68d7138f14fe', '+91', NULL, NULL, true, NULL, NULL, 'ACTIVE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, NULL, NULL, now(), now());

-- Phone number: 9999988888
INSERT INTO atlas_transporter.person (id, first_name, middle_name, last_name, full_name, role, gender, identifier_type, email, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, rating, verified, udf1, udf2, status, organization_id, device_token, location_id, description, created_at, updated_at) VALUES
  ('a30193df-4f7c-440f-bada-4d46c396d7d0', NULL, NULL, NULL, NULL, 'ADMIN', 'UNKNOWN', 'MOBILENUMBER', NULL, '0.1.0|0|BEr9F11LIq8SIoxEwzGp3sD3QWLhty3XYOvxyI5r6H90GIh8BUjXiTKsmj+F15FFICiUhSw4GB8yRj7wtA==', '\x0b0c9417ddba512efba45e716b6f6e7abfbb4307ac4dd8204a58936acfdbac37', '+91', NULL, NULL, true, NULL, NULL, 'ACTIVE', 'e1f37274-f0aa-4bb3-93a0-2476349487b7', NULL, NULL, NULL, now(), now());

INSERT INTO atlas_transporter.person (id, role, gender, verified, status, organization_id, location_id, udf1, udf2, identifier_type) VALUES
  ('001093df-4f7c-440f-b-furthest_driver', 'DRIVER', 'MALE',   true, 'ACTIVE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'furthest_driver', '0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE', 'MOBILENUMBER'),
  ('002093df-4f7c-440f-ba-closest_driver',  'DRIVER', 'MALE',   true, 'ACTIVE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'closest_driver',  '0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE', 'MOBILENUMBER'),
  ('003093df-4f7c-440f-bada-other_driver',    'DRIVER', 'FEMALE', true, 'ACTIVE', 'e1f37274-f0aa-4bb3-93a0-2476349487b7', 'other_driver',    '0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE', 'MOBILENUMBER'),
  ('003093df-4f7c-440f-bada-4-suv_driver',    'DRIVER', 'FEMALE', true, 'ACTIVE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'suv_driver',    'f1b84838-5cb5-4eb1-8f4b-0dd0c4a2-suv', 'VEHICLE', 'MOBILENUMBER'),
  ('003093df-4f7c-440f-bada-sedan_driver',    'DRIVER', 'FEMALE', true, 'ACTIVE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'sedan_driver',    'f1b84838-5cb5-4eb1-8f4b-0dd0c4-sedan', 'VEHICLE', 'MOBILENUMBER'),
  ('003093df-4f7c-440f--hatchback_driver',    'DRIVER', 'FEMALE', true, 'ACTIVE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'hatchback_driver',    'f1b84838-5cb5-4eb1-8f4b-0d-hatchback', 'VEHICLE', 'MOBILENUMBER');

--
-- Data for Name: product; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

INSERT INTO atlas_transporter.product (id, name, industry, type, status, short_id, price, created_at, updated_at) values
  ('998af371-e726-422e-8356-d24085a1d586', 'AUTO', 'MOBILITY', 'RIDE', 'INSTOCK', 'Dney75jyIwsKoR7a', '0.0000000000', now(), now());
INSERT INTO atlas_transporter.product (id, name, industry, type, status, short_id, price, created_at, updated_at) values
  ('5ad086dd-c5c1-49a6-b66d-245d13b70194', 'HATCHBACK', 'MOBILITY', 'RIDE', 'INSTOCK', 'EBL2GZPJAHvaxLRO', '0.0000000000', now(), now());
INSERT INTO atlas_transporter.product (id, name, industry, type, status, short_id, price, created_at, updated_at) values
  ('f726d2fa-2df1-42f0-a009-6795cfdc9b05', 'SUV', 'MOBILITY', 'RIDE', 'INSTOCK', 'SldbUk7Kplnz7B6X', '0.0000000000', now(), now());
INSERT INTO atlas_transporter.product (id, name, industry, type, status, short_id, price, created_at, updated_at) values
  ('ad044fd7-2b62-4f37-93da-e48fe0678de1', 'SEDAN', 'MOBILITY', 'RIDE', 'INSTOCK', 'UINnHxAgoQlqkRrD', '0.0000000000', now(), now());

--
-- Data for Name: registration_token; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

INSERT INTO atlas_transporter.registration_token (id, auth_medium, auth_type, auth_value_hash, token, verified, auth_expiry, token_expiry, attempts, entity_id, entity_type, created_at, updated_at) values
  ('772453e2-d02b-494a-a4ac-ec1ea0027e18', 'SMS', 'OTP', '3249', 'ea37f941-427a-4085-a7d0-96240f166672', true, 3, 365, 3, 'ec34eede-5a3e-4a41-89d4-7290a0d7a629', 'USER', now(), now());

INSERT INTO atlas_transporter.registration_token (id, auth_medium, auth_type, auth_value_hash, token, verified, auth_expiry, token_expiry, attempts, entity_id, entity_type, created_at, updated_at) values
  ('c38562c2-3d58-4b08-8496-drivertoken1', 'SMS', 'OTP', '3249', 'ca05cf3c-c88b-4a2f-8874-drivertoken1', true, 3, 365, 3, '6bc4bc84-2c43-425d-8853-22f47driver1', 'USER', now(), now());
INSERT INTO atlas_transporter.registration_token (id, auth_medium, auth_type, auth_value_hash, token, verified, auth_expiry, token_expiry, attempts, entity_id, entity_type, created_at, updated_at) values
  ('c38562c2-3d58-4b08-8496-drivertoken2', 'SMS', 'OTP', '3249', 'ca05cf3c-c88b-4a2f-8874-drivertoken2', true, 3, 365, 3, '6bc4bc84-2c43-425d-8853-22f47driver2', 'USER', now(), now());

--
-- Data for Name: vehicle; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

INSERT INTO atlas_transporter.vehicle (id, capacity, category, make, model, size, variant, color, energy_type, registration_no, registration_category, organization_id, created_at, updated_at) VALUES
  ('0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94', NULL, NULL, NULL, 'Model1', NULL, 'SUV', 'Black', NULL, '4810', NULL, '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', now(), now());

INSERT INTO atlas_transporter.vehicle (id, capacity, category, make, model, size, variant, color, energy_type, registration_no, registration_category, organization_id, created_at, updated_at) VALUES
  ('f1b84838-5cb5-4eb1-8f4b-0dd0c4a2-suv', NULL, NULL, NULL, 'Model2', NULL, 'SUV', 'Black', NULL, '5613', NULL, 'e1f37274-f0aa-4bb3-93a0-2476349487b7', now(), now()),
  ('f1b84838-5cb5-4eb1-8f4b-0dd0c4-sedan', NULL, NULL, NULL, 'Model2', NULL, 'SEDAN', 'Black', NULL, '5614', NULL, 'e1f37274-f0aa-4bb3-93a0-2476349487b7', now(), now()),
  ('f1b84838-5cb5-4eb1-8f4b-0d-hatchback', NULL, NULL, NULL, 'Model2', NULL, 'HATCHBACK', 'Black', NULL, '5615', NULL, 'e1f37274-f0aa-4bb3-93a0-2476349487b7', now(), now());

--
-- PostgreSQL database dump complete
--

INSERT INTO atlas_transporter.location (id, location_type, lat, long) VALUES
  ('e95d2f36-a455-4625-bfb4-22807fefa1eb', 'POINT', 10.082713, 76.268572);

INSERT INTO atlas_transporter.location (id, location_type, long, lat, point) VALUES
                                                                                         -- lon        lat
  ('furthest_driver', 'POINT', 77.593360, 13.005432, public.ST_SetSRID(public.ST_Point(77.593360, 13.005432), 4326)),
  ('closest_driver',  'POINT', 77.593360, 13.005432, public.ST_SetSRID(public.ST_Point(77.601921, 12.995477), 4326)),
  ('other_driver',    'POINT', 77.593360, 13.005432, public.ST_SetSRID(public.ST_Point(77.601922, 12.995478), 4326)),
  ('suv_driver', 'POINT', 77.593360, 13.005432, public.ST_SetSRID(public.ST_Point(77.593360, 13.055433), 4326)),
  ('sedan_driver', 'POINT', 77.593360, 13.005432, public.ST_SetSRID(public.ST_Point(77.593360, 13.056433), 4326)),
  ('hatchback_driver', 'POINT', 77.593360, 13.005432, public.ST_SetSRID(public.ST_Point(77.593360, 13.057433), 4326));

INSERT INTO atlas_transporter.location (id, location_type, long, lat, point) VALUES
  ('0202b94d-dbf0-4e95-bbf9-25cafc888173', 'POINT', 76.2733, 10.0739, public.ST_SetSRID(public.ST_Point(76.2733, 10.0739), 4326));
INSERT INTO atlas_transporter.location (id, location_type, long, lat, point) VALUES
  ('3b2af603-c6a3-404e-9859-27608driver1', 'POINT', 76.2733, 10.0739, public.ST_SetSRID(public.ST_Point(76.2733, 10.0739), 4326));
INSERT INTO atlas_transporter.location (id, location_type, long, lat, point) VALUES
  ('3b2af603-c6a3-404e-9859-27608driver2', 'POINT', 76.2733, 10.0741, public.ST_SetSRID(public.ST_Point(76.2733, 10.0741), 4326));

INSERT INTO atlas_transporter.driver_information (driver_id, active, on_ride, created_at, updated_at) select id, False, False, now(), now() from atlas_transporter.person where role ='DRIVER';
INSERT INTO atlas_transporter.driver_stats (driver_id, idle_since) SELECT id, now() FROM atlas_transporter.person WHERE role ='DRIVER';

INSERT INTO atlas_transporter.fare_policy (id, vehicle_variant, organization_id, base_fare, base_distance, per_extra_km_rate, night_shift_start, night_shift_end, night_shift_rate) VALUES
  ('9a978cc8-584b-4bd1-ad7f-0c061e25b92d', 'SUV', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 120.0, 5000.0, 12.0, '21:00:00', '5:00:00', 1.1);

INSERT INTO atlas_transporter.fare_policy (id, vehicle_variant, organization_id, base_fare, base_distance, per_extra_km_rate, night_shift_start, night_shift_end, night_shift_rate) VALUES
  ('f777fbfa-53a7-4f68-90b8-b55492f01b8b', 'SUV', 'e1f37274-f0aa-4bb3-93a0-2476349487b7', 120.0, 5000.0, 12.0, '21:00:00', '5:00:00', 1.1);

INSERT INTO atlas_transporter.fare_policy (id, vehicle_variant, organization_id, base_fare, base_distance, per_extra_km_rate, night_shift_start, night_shift_end, night_shift_rate) VALUES
  ('8e36611e-c2d5-4094-80f2-8306fba03d2c', 'SEDAN', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 120.0, 5000.0, 12.0, '21:00:00', '5:00:00', 1.1);

INSERT INTO atlas_transporter.fare_policy (id, vehicle_variant, organization_id, base_fare, base_distance, per_extra_km_rate, night_shift_start, night_shift_end, night_shift_rate) VALUES
  ('7fcf9719-2db4-41ea-8a1e-6922d8b23490', 'SEDAN', 'e1f37274-f0aa-4bb3-93a0-2476349487b7', 120.0, 5000.0, 12.0, '21:00:00', '5:00:00', 1.1);

INSERT INTO atlas_transporter.fare_policy (id, vehicle_variant, organization_id, base_fare, base_distance, per_extra_km_rate, night_shift_start, night_shift_end, night_shift_rate) VALUES
  ('9ec95f54-1ad5-4701-8c74-c935b24af0fb', 'HATCHBACK', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 120.0, 5000.0, 12.0, '21:00:00', '5:00:00', 1.1);

INSERT INTO atlas_transporter.fare_policy (id, vehicle_variant, organization_id, base_fare, base_distance, per_extra_km_rate, night_shift_start, night_shift_end, night_shift_rate) VALUES
  ('c66eb8af-496d-4042-abf1-8fb2dd194800', 'HATCHBACK', 'e1f37274-f0aa-4bb3-93a0-2476349487b7', 120.0, 5000.0, 12.0, '21:00:00', '5:00:00', 1.1);

-- Driver1
INSERT INTO atlas_transporter.vehicle (id, capacity, category, make, model, size, variant, color, energy_type, registration_no, registration_category, organization_id, created_at, updated_at) VALUES
  ('001cd0bc-b3a4-4c6c-811f-900ccf4dfb94', NULL, NULL, NULL, 'Model3', NULL, 'SUV', 'WHITE', NULL, '4277', NULL, '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', now(), now());

INSERT INTO atlas_transporter.location (id, location_type, long, lat, point) VALUES
  ('001d2f36-a455-4625-bfb4-22807fefa1eb', 'POINT', 76.2733, 10, public.ST_SetSRID(public.ST_Point(76.2733, 10), 4326));

INSERT INTO atlas_transporter.person (id, first_name, middle_name, last_name, full_name, role, gender, identifier_type, email,password_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, rating, verified, udf1, udf2, status, organization_id, device_token, location_id, description, created_at, updated_at) VALUES
  ('001b93df-4f7c-440f-bada-4d46c396d7d0', 'Some', 'Cool', 'Driver', NULL, 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, NULL,  '0.1.0|0|iP3CepsEe8Qmw1xbLR5HJFSESfdvU2tWtNWrdCZWtwp4msTfh1BDkc95/yytpllMp61Q8mpiS+KDde+Plw==', '\xa0a56e902b973e6cf231520c2acbda9b44947dd3a88fb0daacd23d68082c6362', '+92', '001', NULL, true, '001cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE', 'ACTIVE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, '001d2f36-a455-4625-bfb4-22807fefa1eb', NULL, now(), now());

INSERT INTO atlas_transporter.registration_token (id, auth_medium, auth_type, auth_value_hash, token, verified, auth_expiry, token_expiry, attempts, entity_id, entity_type, created_at, updated_at) values
  ('001d53e2-d02b-494a-a4ac-ec1ea0027e18', 'SMS', 'OTP', '1233', '001df941-427a-4085-a7d0-96240f166672', true, 3, 365, 3, '001b93df-4f7c-440f-bada-4d46c396d7d0', 'USER', now(), now());

-- Driver2
INSERT INTO atlas_transporter.vehicle (id, capacity, category, make, model, size, variant, color, energy_type, registration_no, registration_category, organization_id, created_at, updated_at) VALUES
  ('002cd0bc-b3a4-4c6c-811f-900ccf4dfb94', NULL, NULL, NULL, 'Model4', NULL, 'SUV', 'GREEN', NULL, '3211', NULL, '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', now(), now());

INSERT INTO atlas_transporter.location (id, location_type, long, lat, point) VALUES
  ('002d2f36-a455-4625-bfb4-22807fefa1eb', 'POINT', 76.2733, 10, public.ST_SetSRID(public.ST_Point(76.2733, 10), 4326));

INSERT INTO atlas_transporter.person (id, first_name, middle_name, last_name, full_name, role, gender, identifier_type, email, password_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, rating, verified, udf1, udf2, status, organization_id, device_token, location_id, description, created_at, updated_at) VALUES
  ('002b93df-4f7c-440f-bada-4d46c396d7d0', 'Another', 'Cool', 'Driver', NULL, 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, NULL,  '0.1.0|0|iP3CepsEe8Qmw1xbLR5HJFSESfdvU2tWtNWrdCZWtwp4msTfh1BDkc95/yytpllMp61Q8mpiS+KDde+Plw==', '\xa0a56e902b973e6cf231520c2acbda9b44947dd3a88fb0daacd23d68082c6362', '+93', '002', NULL, true, '002cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE', 'ACTIVE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, '002d2f36-a455-4625-bfb4-22807fefa1eb', NULL, now(), now());

INSERT INTO atlas_transporter.registration_token (id, auth_medium, auth_type, auth_value_hash, token, verified, auth_expiry, token_expiry, attempts, entity_id, entity_type, created_at, updated_at) values
  ('002d53e2-d02b-494a-a4ac-ec1ea0027e18', 'SMS', 'OTP', '1234', '002df941-427a-4085-a7d0-96240f166672', true, 3, 365, 3, '002b93df-4f7c-440f-bada-4d46c396d7d0', 'USER', now(), now());

INSERT INTO atlas_transporter.driver_information (driver_id, active, on_ride, created_at, updated_at) select id, False, False, now(), now() from atlas_transporter.person where id ='001b93df-4f7c-440f-bada-4d46c396d7d0';
INSERT INTO atlas_transporter.driver_information (driver_id, active, on_ride, created_at, updated_at) select id, False, False, now(), now() from atlas_transporter.person where id ='002b93df-4f7c-440f-bada-4d46c396d7d0';
INSERT INTO atlas_transporter.driver_stats (driver_id, idle_since) SELECT id, now() FROM atlas_transporter.person WHERE id ='001b93df-4f7c-440f-bada-4d46c396d7d0';
INSERT INTO atlas_transporter.driver_stats (driver_id, idle_since) SELECT id, now() FROM atlas_transporter.person WHERE id ='002b93df-4f7c-440f-bada-4d46c396d7d0';
