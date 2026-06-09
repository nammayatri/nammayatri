-- Toll dashboard access_matrix for JUSPAY_ADMIN integration tests (idempotent).
-- Applied by test-context-api startup (local-testing-data batch) and ./run-tests.sh toll-*.

-- Provider dashboard (BPP)
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_TOLL_UPSERT' )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_TOLL_LIST' )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_TOLL_UPSERT' )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_TOLL_DELETE' )
ON CONFLICT DO NOTHING;

-- Rider dashboard (BAP)
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_TOLL_UPSERT' )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_TOLL_LIST' )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_TOLL_UPSERT' )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_TOLL_DELETE' )
ON CONFLICT DO NOTHING;
