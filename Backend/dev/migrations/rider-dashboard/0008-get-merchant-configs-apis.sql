-- for testing purposes
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        'SERVICE_USAGE_CONFIG'
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'MERCHANT' AND T1.user_access_type = 'USER_FULL_ACCESS' AND T1.user_action_type = 'MAPS_SERVICE_USAGE_CONFIG_UPDATE'
    )
ON CONFLICT DO NOTHING;
