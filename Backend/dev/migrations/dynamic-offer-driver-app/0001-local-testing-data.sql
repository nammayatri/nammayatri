--- ONLY FOR LOCAL SYNC | Dont run in master or prod
ALTER TABLE atlas_driver_offer_bpp.merchant ADD COLUMN domain text;
ALTER TABLE atlas_driver_offer_bpp.merchant ADD COLUMN type text;
ALTER TABLE atlas_driver_offer_bpp.merchant ADD COLUMN api_key text;
ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN origin_restriction DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN destination_restriction DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN short_id DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN state DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN country DROP NOT NULL;


-- ONLY LOCAL SYNC
INSERT INTO atlas_driver_offer_bpp.merchant (id, name, subscriber_id, gstin, status, type, domain, verified, enabled, description, mobile_number, mobile_country_code, from_time, to_time, api_key, head_count, created_at, updated_at, info, unique_key_id, internal_api_key) VALUES
	('1926d40f-1223-4eb2-ba5d-7983bde2fd02', 'Juspay Gateway', 'JUSPAY.BG.1', NULL, 'APPROVED', 'GATEWAY', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, '2022-04-12 15:15:42.213874+00', '2022-04-12 15:15:42.213874+00', NULL, 'FIXME','test-api-key-for-dev-env'),
	('0d7016d0-f9cd-4f9f-886f-bc4cbd6a86e5', 'NSDL Gateway', 'NSDL', NULL, 'APPROVED', 'GATEWAY', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, '2022-04-12 15:15:42.215812+00', '2022-04-12 15:15:42.215812+00', NULL, 'FIXME', 'test-api-key-for-dev-env'),
	('505e4651-5340-4836-81a7-045394ba6dc3', 'Mobility BAP', 'NAMMA_YATRI_BAP', NULL, 'APPROVED', 'APP', 'MOBILITY', true, true, NULL, '9777777777', '+91', NULL, NULL, NULL, NULL, '2022-04-12 15:15:42.21825+00', '2022-04-12 15:15:42.21825+00', NULL, 'FIXME', 'test-api-key-for-dev-env'),
	('239ee68b-0818-4dba-ad31-032fe809cf71', 'NSDL Gateway', 'NSDL.BG.1', NULL, 'APPROVED', 'GATEWAY', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, '2022-04-12 15:15:42.21902+00', '2022-04-12 15:15:42.21902+00', NULL, 'FIXME', 'test-api-key-for-dev-env'),
	('favorit0-0000-0000-0000-00000favorit', 'Driver-Offer-Provider #1', 'NAMMA_YATRI', NULL, 'APPROVED', 'PROVIDER', 'MOBILITY', true, true, NULL, '9888888888', '+91', NULL, NULL, NULL, NULL, '2022-04-12 15:15:42.216602+00', '2022-04-12 15:15:42.216602+00', NULL, 'juspay-mobility-bpp-1-key', 'test-api-key-for-dev-env'),
	('nearest-drivers-testing-organization', 'Driver-Offer-Provider #2', 'NAMMA_YATRI_2', NULL, 'APPROVED', 'PROVIDER', 'MOBILITY', true, true, NULL, '9888888777', '+91', NULL, NULL, NULL, NULL, '2022-04-12 15:15:42.216602+00', '2022-04-12 15:15:42.216602+00', NULL, 'juspay-mobility-bpp-1-key', 'test-api-key-for-dev-env');

-- ONLY FOR LOCAL
UPDATE atlas_driver_offer_bpp.merchant SET
  short_id = id;
ALTER TABLE atlas_driver_offer_bpp.merchant
  ALTER COLUMN short_id SET NOT NULL;




INSERT INTO atlas_driver_offer_bpp.person (id, first_name, middle_name, last_name, role, gender, identifier_type, email, password_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, is_new, device_token, description, created_at, updated_at, rating, merchant_id, onboarded_from_dashboard, total_earned_coins,used_coins) VALUES
    ('favorit-suv-000000000000000000000000', 'Robert', 'James', 'Fischer', 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, NULL, '0.1.0|0|iP3CepsEe8Qmw1xbLR5HJFSESfdvU2tWtNWrdCZWtwp4msTfh1BDkc95/yytpllMp61Q8mpiS+KDde+Plw==', '\xa0a56e902b973e6cf231520c2acbda9b44947dd3a88fb0daacd23d68082c6362', '+91', NULL, true, 'favorit-suv-000000000000device-token', NULL, '2022-04-12 15:15:42.21973+00', '2022-04-12 15:15:42.21973+00', NULL,'favorit0-0000-0000-0000-00000favorit', false, 0, 0),
    ('favorit-sedan-0000000000000000000000', 'Pentala', NULL, 'Harikrishna', 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, NULL, '0.1.0|0|iP3CepsEe8Qmw1xbLR5HJFSESfdvU2tWtNWrdCZWtwp4msTfh1BDkc95/yytpllMp61Q8mpiS+KDde+Plw==', '\xa0a56e902b973e6cf231520c2acbda9b44947dd3a88fb0daacd23d68082c6362', '+94', NULL, true, 'favorit-sedan-0000000000device-token', NULL, '2022-04-12 15:15:42.221002+00', '2022-04-12 15:15:42.221002+00', NULL, 'favorit0-0000-0000-0000-00000favorit', false, 0, 0),
    ('favorit-hatchback-000000000000000000', 'Stefan', NULL, 'Banach', 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, NULL, NULL, NULL, '+91', NULL, true, 'favorit-hatchback-000000device-token', NULL, '2022-04-12 15:15:42.222142+00', '2022-04-12 15:15:42.222142+00', NULL, 'favorit0-0000-0000-0000-00000favorit', false, 0, 0),
    ('favorit-auto1-0000000000000000000000', 'Ramesh', NULL, 'Gowda', 'DRIVER', 'MALE', 'MOBILENUMBER', 'Ramesh@gmail.com', NULL, '0.1.0|0|iP3CepsEe8Qmw1xbLR5HJFSESfdvU2tWtNWrdCZWtwp4msTfh1BDkc95/yytpllMp61Q8mpiS+KDde+Plw==', '\xa0a56e902b973e6cf231520c2acbda9b44947dd3a88fb0daacd23d68082c6362', '+92', NULL, true, 'favorit-auto1-0000000000device-token', NULL, '2022-04-12 15:15:42.222142+00', '2022-04-12 15:15:42.222142+00', NULL,'favorit0-0000-0000-0000-00000favorit', false, 0, 0),
    ('favorit-auto2-0000000000000000000000', 'Krishna', NULL, 'Sagar', 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, NULL, NULL, NULL, '+91', NULL, true, 'favorit-auto2-0000000000device-token', NULL, '2022-04-12 15:15:42.222142+00', '2022-04-12 15:15:42.222142+00', NULL,'favorit0-0000-0000-0000-00000favorit', false, 0, 0),
    ('favorit-admin-0000000000000000000000', '', NULL, NULL, 'ADMIN', 'UNKNOWN', 'MOBILENUMBER', NULL, NULL, '0.1.0|1|8hdNzNRjeq0j7QRZoKzT2iMjoKkQJzwLoUeOEF9edh1wmRer7NbUaUT3foUI1wG2gZMjpB8vDuUKUDG+aQ==', '\xba0f72d2d550eb699914591dab2eadeb8fc83dd1eda6bde7cc3f68d7138f14fe', '+91', NULL, false, NULL, NULL, '2022-04-12 15:15:42.223341+00', '2022-04-12 15:15:42.223341+00', NULL,'favorit0-0000-0000-0000-00000favorit', false, 0, 0);




INSERT INTO atlas_driver_offer_bpp.driver_information (driver_id, active, on_ride, created_at, updated_at, enabled) VALUES
  ('favorit-suv-000000000000000000000000', false, false, '2022-04-12 15:15:42.280941+00', '2022-04-12 15:15:42.280941+00', true),
  ('favorit-sedan-0000000000000000000000', false, false, '2022-04-12 15:15:42.280941+00', '2022-04-12 15:15:42.280941+00', true),
  ('favorit-hatchback-000000000000000000', false, false, '2022-04-12 15:15:42.280941+00', '2022-04-12 15:15:42.280941+00', true),
  ('favorit-auto1-0000000000000000000000', false, false, '2022-04-12 15:15:42.280941+00', '2022-04-12 15:15:42.280941+00', true),
  ('favorit-auto2-0000000000000000000000', false, false, '2022-04-12 15:15:42.280941+00', '2022-04-12 15:15:42.280941+00', true);


INSERT INTO atlas_driver_offer_bpp.driver_location (driver_id, lat, lon, point, created_at, updated_at) VALUES
	('favorit-suv-000000000000000000000000', 10.0739, 76.2733, '0101000020E6100000CC7F48BF7D1153404B598638D6252440', '2022-04-12 15:15:42.279179+00', now()),
	('favorit-sedan-0000000000000000000000', 10.0741, 76.2733, '0101000020E6100000CC7F48BF7D1153406744696FF0252440', '2022-04-12 15:15:42.280142+00', now()),
	('favorit-hatchback-000000000000000000', 10.0739, 76.2733, '0101000020E6100000CC7F48BF7D1153404B598638D6252440', '2022-04-12 15:15:42.27825+00', now()),
	('favorit-auto1-0000000000000000000000', 10.0739, 76.2733, '0101000020E6100000CC7F48BF7D1153403B598638D6252440', '2022-04-12 15:15:42.27825+00', now()),
	('favorit-auto2-0000000000000000000000', 10.0739, 76.2733, '0101000020E6100000CC7F48BF7D1153402B598638D6252440', '2022-04-12 15:15:42.27825+00', now());

INSERT INTO atlas_driver_offer_bpp.driver_stats (driver_id, idle_since) VALUES
	('favorit-suv-000000000000000000000000', '2022-04-12 15:15:42.283174+00'),
	('favorit-sedan-0000000000000000000000', '2022-04-12 15:15:42.283174+00'),
	('favorit-hatchback-000000000000000000', '2022-04-12 15:15:42.283174+00'),
	('favorit-auto1-0000000000000000000000', '2022-04-12 15:15:42.283174+00'),
	('favorit-auto2-0000000000000000000000', '2022-04-12 15:15:42.283174+00');


INSERT INTO atlas_driver_offer_bpp.registration_token (id, auth_medium, auth_type, auth_value_hash, token, verified, auth_expiry, token_expiry, attempts, entity_id, entity_type, info, created_at, updated_at, merchant_id) VALUES
	('favorit-admin-0000000000000000000000', 'SMS', 'OTP', '3249', 'favorit-admin-0000000000000000-token', true, 3, 365, 3, 'favorit-admin-0000000000000000000000', 'USER                                ', NULL, now (), now (), 'favorit0-0000-0000-0000-00000favorit'),
	('favorit-suv-000000000000000000000000', 'SMS', 'OTP', '3249', 'favorit-suv-000000000000000000-token', true, 3, 365, 3, 'favorit-suv-000000000000000000000000', 'USER                                ', NULL, now (), now (), 'favorit0-0000-0000-0000-00000favorit'),
	('favorit-sedan-0000000000000000000000', 'SMS', 'OTP', '3249', 'favorit-sedan-0000000000000000-token', true, 3, 365, 3, 'favorit-sedan-0000000000000000000000', 'USER                                ', NULL, now (), now (), 'favorit0-0000-0000-0000-00000favorit'),
	('favorit-hatchback-000000000000000000', 'SMS', 'OTP', '3249', 'favorit-hatchback-000000000000-token', true, 3, 365, 3, 'favorit-hatchback-000000000000000000', 'USER                                ', NULL, now (), now (), 'favorit0-0000-0000-0000-00000favorit'),
	('favorit-auto1-0000000000000000000000', 'SMS', 'OTP', '1233', 'favorit-auto1-0000000000000000-token', true, 3, 365, 3, 'favorit-auto1-0000000000000000000000', 'USER                                ', NULL, now (), now (), 'favorit0-0000-0000-0000-00000favorit'),
	('favorit-auto2-0000000000000000000000', 'SMS', 'OTP', '1234', 'favorit-auto2-0000000000000000-token', true, 3, 365, 3, 'favorit-auto2-0000000000000000000000', 'USER                                ', NULL, now (), now (), 'favorit0-0000-0000-0000-00000favorit');

INSERT INTO atlas_driver_offer_bpp.fare_policy
  (id, organization_id, base_fare, night_shift_start, night_shift_end,
    night_shift_rate, created_at, updated_at) VALUES
  ('0991cec4-72d4-40f6-8ddd-c77a97c3b898', 'favorit0-0000-0000-0000-00000favorit', 120, NULL, NULL, 1, now(), now()),
  ('002d53e2-d02b-494a-a4ac-ec1ea0027e18', 'nearest-drivers-testing-organization', 120, NULL, NULL, 1, now(), now());

INSERT INTO atlas_driver_offer_bpp.fare_policy_per_extra_km_rate
  (id, organization_id, distance_range_start, fare) VALUES
  ('0991cec4-72d4-40f6-8ddd-c77a97c3b897', 'favorit0-0000-0000-0000-00000favorit', 5000, 12),
  ('002d53e2-d02b-494a-a4ac-ec1ea0027e18', 'nearest-drivers-testing-organization', 5000, 12);
INSERT INTO atlas_driver_offer_bpp.vehicle (driver_id, capacity, category, make, model, size, variant, color, energy_type, registration_no, registration_category, vehicle_name, vehicle_class, merchant_id, created_at, updated_at) VALUES
	('favorit-suv-000000000000000000000000', 5, NULL, NULL, 'Tahoe', NULL, 'SUV', 'Green', NULL, '4810', NULL, NULL, '3WT', 'favorit0-0000-0000-0000-00000favorit', '2022-04-12 15:15:42.233691+00', '2022-04-12 15:15:42.233691+00'),
	('favorit-sedan-0000000000000000000000', 5, NULL, NULL, 'Crown Majesta', NULL, 'SEDAN', 'Black', NULL, '4811', NULL, NULL, '3WT', 'favorit0-0000-0000-0000-00000favorit', '2022-04-12 15:15:42.233691+00', '2022-04-12 15:15:42.233691+00'),
	('favorit-hatchback-000000000000000000', 4, NULL, NULL, 'Civic', NULL, 'HATCHBACK', 'Red', NULL, '4812', NULL, NULL, '3WT', 'favorit0-0000-0000-0000-00000favorit', '2022-04-12 15:15:42.233691+00', '2022-04-12 15:15:42.233691+00'),
	('favorit-auto1-0000000000000000000000', 3, NULL, NULL, 'Auto1', NULL, 'AUTO_RICKSHAW', 'Yellow', NULL, '4813', NULL, NULL, '3WT', 'favorit0-0000-0000-0000-00000favorit', '2022-04-12 15:15:42.233691+00', '2022-04-12 15:15:42.233691+00'),
	('favorit-auto2-0000000000000000000000', 3, NULL, NULL, 'Auto2', NULL, 'AUTO_RICKSHAW', 'Yellow', NULL, '4814', NULL, NULL, '3WT', 'favorit0-0000-0000-0000-00000favorit', '2022-04-12 15:15:42.233691+00', '2022-04-12 15:15:42.233691+00');
