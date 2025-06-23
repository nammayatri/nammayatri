INSERT INTO atlas_bap_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at) VALUES
    ('e5a69a26-d165-455a-a711-33a41e0d4814', 'CONFIG_MANAGER', 'DASHBOARD_USER', 'config manager for running config pilot APIs', '2025-03-03 09:34:13.718955+00', '2025-03-03 09:34:13.718955+00');

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4814', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_VERSION' ) ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4814', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_CONFIG' ) ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4814', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_CREATE_UI_CONFIG' ) ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4814', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_ALL_CONFIGS' ) ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4814', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_CONFIG_DETAILS' ) ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4814', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_GET_TABLE_DATA' ) ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4814', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_ACTION_CHANGE' ) ON CONFLICT DO NOTHING;