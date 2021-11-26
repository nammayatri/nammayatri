--
-- TOC entry 3000 (class 0 OID 16410)
-- Dependencies: 206
-- Data for Name: organization; Type: TABLE DATA; Schema: atlas_gateway; Owner: atlas
--

INSERT INTO atlas_gateway.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('test-provider-2                     ', 'Test provider 2', 'JUSPAY.BPP.MOCK.1', NULL, 'APPROVED', 'PROVIDER', 'FINAL_MILE_DELIVERY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'http://localhost:8017/v1', NULL, now(), now(), NULL, NULL);
INSERT INTO atlas_gateway.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('provider-wrapper                    ', 'Fmd wrapper', 'fmd-wrapper', NULL, 'APPROVED', 'PROVIDER', 'FINAL_MILE_DELIVERY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'http://localhost:8018/v1', NULL, now(), now(), NULL, NULL);
INSERT INTO atlas_gateway.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('test-app-2                          ', 'Test App 2', 'JUSPAY.BAP.MOCK.1', NULL, 'APPROVED', 'APP', 'FINAL_MILE_DELIVERY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'http://localhost:8016/v1', NULL, now(), now(), NULL, NULL);
INSERT INTO atlas_gateway.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('fmd-test-app                        ', 'FMD Test App', 'fmd-test-app', NULL, 'APPROVED', 'APP', 'FINAL_MILE_DELIVERY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'http://localhost:8019/v1', NULL, now(), now(), NULL, NULL);
INSERT INTO atlas_gateway.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('a30193df-4f7c-440f-bada-4d46c396d7d0', '[G] Transporter #1', 'JUSPAY.MOBILITY.PROVIDER.UAT.1', NULL, 'APPROVED', 'PROVIDER', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'http://localhost:8014/v1/7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, now(), now(), NULL, NULL);
INSERT INTO atlas_gateway.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('83dde90a-81d2-404b-ada5-20aac58005e6', '[G] Transporter #2', 'another-test-cabs', NULL, 'APPROVED', 'PROVIDER', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'http://localhost:8014/v1/e1f37274-f0aa-4bb3-93a0-2476349487b7', NULL, now(), now(), NULL, NULL);

INSERT INTO atlas_gateway.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('9b59f07a-bf3a-49db-98ce-fffc71a6c2bd', 'Metro BPP', 'metro-bpp', NULL, 'APPROVED', 'PROVIDER', 'METRO', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'http://localhost:8000', NULL, now(), now(), NULL, NULL);
    ('b4810ce6-07ed-4e3e-b2ae-50e0d214858d', 'Parking BPP', 'parking-bpp', NULL, 'APPROVED', 'PROVIDER', 'PARKING', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'http://localhost:9090', NULL, now(), now(), NULL, NULL);
    
INSERT INTO atlas_gateway.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES
    ('mobility-app                        ', 'Mobility app', 'JUSPAY.MOBILITY.APP.UAT.1', NULL, 'APPROVED', 'APP', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'http://localhost:8013/cab/v1', NULL, now(), now(), NULL, NULL),
    ('mobility-app-metro                  ', 'Mobility app', 'JUSPAY.MOBILITY.APP.UAT.2', NULL, 'APPROVED', 'APP', 'METRO', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'http://localhost:8013/metro/v1', NULL, now(), now(), NULL, NULL),
    ('mobility-app-parking                ', 'Mobility app', 'JUSPAY.MOBILITY.APP.UAT.3', NULL, 'APPROVED', 'APP', 'PARKING', true, true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'http://localhost:8013/parking', NULL, now(), now(), NULL, NULL);
