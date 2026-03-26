--  LOCAL ONLY
INSERT INTO atlas_registry.subscriber (unique_key_id, subscriber_id, subscriber_url, type, domain, city, country, status, signing_public_key, encr_public_key, valid_from, valid_until, created, updated) VALUES
	('metro-bpp-key', 'metro-bpp', 'http://localhost:8000', 'BPP', 'METRO', ARRAY['Kochi'], NULL, NULL, 'OGfSqt352PXRfdd+pLXo3eLLd96iL8dcbireMynl5A4=', NULL, NULL, NULL, '2022-04-12 15:15:42.441694+00', '2022-04-12 15:15:42.441694+00'),
	('nsdl_bg_1', 'NSDL.BG.1', 'https://gateway-1.beckn.nsdl.co.in/v1', 'BG', 'MOBILITY', ARRAY['Kochi'], NULL, NULL, 'Fhjwaka1Za+ld+7Nms7S0C675r24mZoyWVn8JbYTjSs=', NULL, NULL, NULL, '2022-04-12 15:15:42.441694+00', '2022-04-12 15:15:42.441694+00'),
	('juspay-mobility-bap-1-key', 'JUSPAY.MOBILITY.APP.UAT.2', 'http://localhost:8013/metro/v1', 'BAP', 'METRO', ARRAY['Kochi'], NULL, NULL, '1tRC4LHVKWeGikY5C8T1FYI9t3oacpa3IFvj8e7aGws=', NULL, NULL, NULL, '2022-04-12 15:15:42.441694+00', '2022-04-12 15:15:42.441694+00'),
	('juspay-mobility-bap-1-key', 'localhost/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a51', 'http://localhost:8013/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a51', 'BAP', 'MOBILITY', ARRAY['Kochi'], NULL, NULL, '1tRC4LHVKWeGikY5C8T1FYI9t3oacpa3IFvj8e7aGws=', NULL, NULL, NULL, '2022-04-12 15:15:42.441694+00', '2022-04-12 15:15:42.441694+00'),
	('juspay-fmd-1-key', 'JUSPAY.FMD.UAT.1', 'http://localhost:8018/v1', 'BPP', 'LOGISTICS', ARRAY['Kochi'], NULL, NULL, '1tRC4LHVKWeGikY5C8T1FYI9t3oacpa3IFvj8e7aGws=', NULL, NULL, NULL, '2022-04-12 15:15:42.441694+00', '2022-04-12 15:15:42.441694+00'),
	('mock-bap-key', 'MOCK.BAP.UAT.1', 'http://localhost:8027/', 'BAP', 'MOBILITY', ARRAY['Kochi'], NULL, NULL, '1tRC4LHVKWeGikY5C8T1FYI9t3oacpa3IFvj8e7aGws=', NULL, NULL, NULL, '2022-04-12 15:15:42.441694+00', '2022-04-12 15:15:42.441694+00'),
	('juspay-bg-1-key', 'JUSPAY.BG.1', 'http://localhost:8015/v1', 'BG', 'MOBILITY', ARRAY['Kochi'], NULL, NULL, '1tRC4LHVKWeGikY5C8T1FYI9t3oacpa3IFvj8e7aGws=', NULL, NULL, NULL, '2022-04-12 15:15:42.441694+00', '2022-04-12 15:15:42.441694+00'),
	('fmd-test-app-key', 'fmd-test-app', 'http://localhost:8019/v1', 'BPP', 'LOGISTICS', ARRAY['Kochi'], NULL, NULL, '1tRC4LHVKWeGikY5C8T1FYI9t3oacpa3IFvj8e7aGws=', NULL, NULL, NULL, '2022-04-12 15:15:42.441694+00', '2022-04-12 15:15:42.441694+00'),
	('juspay-mobility-bap-1-key', 'localhost/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'http://localhost:8013/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'BAP', 'MOBILITY', ARRAY['Kochi'], NULL, NULL, '1tRC4LHVKWeGikY5C8T1FYI9t3oacpa3IFvj8e7aGws=', NULL, NULL, NULL, '2022-04-12 15:15:42.441694+00', '2022-04-12 15:15:42.441694+00'),
	('juspay-mobility-bpp-1-key1', 'mock-public-transport-bpp', 'http://localhost:8091', 'BPP', 'PUBLIC_TRANSPORT', ARRAY['Kochi'], NULL, NULL, '1tRC4LHVKWeGikY5C8T1FYI9t3oacpa3IFvj8e7aGws=', NULL, NULL, NULL, '2022-04-12 15:15:42.441694+00', '2022-04-12 15:15:42.441694+00'),
	('juspay-mobility-bap-1-key', 'JUSPAY.PUBLIC_TRANSPORT.APP.UAT.1', 'http://localhost:8023/beckn', 'BAP', 'PUBLIC_TRANSPORT', ARRAY['Kochi'], NULL, NULL, '1tRC4LHVKWeGikY5C8T1FYI9t3oacpa3IFvj8e7aGws=', NULL, NULL, NULL, '2022-04-12 15:15:42.441694+00', '2022-04-12 15:15:42.441694+00'),
  ('juspay-mobility-bpp-1-key', 'NAMMA_YATRI', 'http://localhost:8016/beckn/favorit0-0000-0000-0000-00000favorit', 'BPP', 'MOBILITY', ARRAY['Kochi'], NULL, NULL, '1tRC4LHVKWeGikY5C8T1FYI9t3oacpa3IFvj8e7aGws=', NULL, NULL, NULL, now(), now());
  --('juspay-mobility-bpp-1-key', 'NAMMA_YATRI_2', 'http://localhost:8016/beckn/nearest-drivers-testing-organization', 'BPP', 'MOBILITY', ARRAY['Kochi'], NULL, NULL, '1tRC4LHVKWeGikY5C8T1FYI9t3oacpa3IFvj8e7aGws=', NULL, NULL, NULL, now(), now());

-- =============================================================================
-- Mock Registry: BAP + BPP subscribers for new merchants
-- Required for Beckn gateway signature verification and BPP routing
-- =============================================================================

-- BAP subscribers (rider-app endpoints)
-- City codes: Kochi = std:0484, Delhi = std:011, Helsinki = fin:009
INSERT INTO atlas_registry.subscriber (unique_key_id, subscriber_id, subscriber_url, type, domain, city, signing_public_key, created, updated)
SELECT 'lynx-bap-key', 'lynx-bap-id',
  'http://localhost:8013/beckn/cab/v1/lynx-bap-0000-0000-0000-000000000000',
  'BAP', 'MOBILITY', '{fin:009}', signing_public_key, now(), now()
FROM atlas_registry.subscriber WHERE unique_key_id = 'juspay-mobility-bap-1-key' AND type = 'BAP' LIMIT 1
ON CONFLICT (unique_key_id, subscriber_id) DO UPDATE SET city = EXCLUDED.city, subscriber_url = EXCLUDED.subscriber_url;

INSERT INTO atlas_registry.subscriber (unique_key_id, subscriber_id, subscriber_url, type, domain, city, signing_public_key, created, updated)
SELECT 'bharat-taxi-bap-key', 'bharat-taxi-bap-id',
  'http://localhost:8013/beckn/cab/v1/bharat-t-bap0-0000-0000-000000000000',
  'BAP', 'MOBILITY', '{std:011}', signing_public_key, now(), now()
FROM atlas_registry.subscriber WHERE unique_key_id = 'juspay-mobility-bap-1-key' AND type = 'BAP' LIMIT 1
ON CONFLICT (unique_key_id, subscriber_id) DO UPDATE SET city = EXCLUDED.city, subscriber_url = EXCLUDED.subscriber_url;

-- BPP subscribers (driver-app endpoints)
INSERT INTO atlas_registry.subscriber (unique_key_id, subscriber_id, subscriber_url, type, domain, city, signing_public_key, created, updated)
SELECT 'juspay-mobility-bpp-1-key', 'LYNX_PARTNER',
  'http://localhost:8016/beckn/lynx-bpp-0000-0000-0000-000000000000',
  'BPP', 'MOBILITY', '{fin:009}', signing_public_key, now(), now()
FROM atlas_registry.subscriber WHERE unique_key_id = 'juspay-mobility-bpp-1-key' AND type = 'BPP' LIMIT 1
ON CONFLICT (unique_key_id, subscriber_id) DO UPDATE SET city = EXCLUDED.city, subscriber_url = EXCLUDED.subscriber_url;

INSERT INTO atlas_registry.subscriber (unique_key_id, subscriber_id, subscriber_url, type, domain, city, signing_public_key, created, updated)
SELECT 'juspay-mobility-bpp-1-key', 'BHARAT_TAXI_PARTNER',
  'http://localhost:8016/beckn/bharat-t-bpp0-0000-0000-000000000000',
  'BPP', 'MOBILITY', '{std:011}', signing_public_key, now(), now()
FROM atlas_registry.subscriber WHERE unique_key_id = 'juspay-mobility-bpp-1-key' AND type = 'BPP' LIMIT 1
ON CONFLICT (unique_key_id, subscriber_id) DO UPDATE SET city = EXCLUDED.city, subscriber_url = EXCLUDED.subscriber_url;
