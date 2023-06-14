-- for testing purposes
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        'MERCHANT_COMMON_CONFIG'
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'MERCHANT' AND T1.user_access_type = 'USER_FULL_ACCESS' AND T1.user_action_type = 'MERCHANT_COMMON_CONFIG_UPDATE'
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        'DRIVER_POOL_CONFIG'
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'MERCHANT' AND T1.user_access_type = 'USER_FULL_ACCESS' AND T1.user_action_type = 'DRIVER_POOL_CONFIG_UPDATE'
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        'DRIVER_INTELLIGENT_POOL_CONFIG'
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'MERCHANT' AND T1.user_access_type = 'USER_FULL_ACCESS' AND T1.user_action_type = 'DRIVER_INTELLIGENT_POOL_CONFIG_UPDATE'
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        'ONBOARDING_DOCUMENT_CONFIG'
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'MERCHANT' AND T1.user_access_type = 'USER_FULL_ACCESS' AND T1.user_action_type = 'ONBOARDING_DOCUMENT_CONFIG_UPDATE'
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        'SERVICE_USAGE_CONFIG'
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'MERCHANT' AND T1.user_access_type = 'USER_FULL_ACCESS' AND T1.user_action_type = 'MAPS_SERVICE_USAGE_CONFIG_UPDATE'
    )
ON CONFLICT DO NOTHING;
