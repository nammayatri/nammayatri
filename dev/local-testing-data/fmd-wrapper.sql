--
-- TOC entry 3000 (class 0 OID 16410)
-- Dependencies: 206
-- Data for Name: organization; Type: TABLE DATA; Schema: atlas_fmd_wrapper; Owner: atlas
--

INSERT INTO atlas_fmd_wrapper.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES 
  ('1926d40f-1223-4eb2-ba5d-7983bde2fd02', 'Gateway', 'gateway', NULL, 'APPROVED', 'GATEWAY', 'FINAL_MILE_DELIVERY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'test-bpp-key0', 'http://localhost:8015/v1', NULL, '2020-08-01 18:37:00+00', '2020-08-01 18:37:00+00', 'fmd-wrapper-key0', NULL);
INSERT INTO atlas_fmd_wrapper.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) VALUES 
  ('747a711c-9537-4f42-8e04-33ded42a9d89', 'FMD Test App', 'fmd-test-app', NULL, 'APPROVED', 'APP', 'FINAL_MILE_DELIVERY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'fmd-test-app-key', 'http://localhost:8019/v1', NULL, '2020-08-01 18:37:00+00', '2020-08-01 18:37:00+00', 'fmd-wrapper-key', '{"dzClientId":"7db7c5e4-5597-45f3-8dae-7d9a7056fb79","dzClientSecret":"3a820bf8-cc91-4c93-92b9-d5e80e67aa9f","bapId":"fmd-test-app"}');
