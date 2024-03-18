INSERT INTO atlas_bap_dashboard.access_matrix
    (id, role_id, api_entity, user_action_type, user_access_type, created_at, updated_at)
(SELECT atlas_bap_dashboard.uuid_generate_v4() as id,
        id as role_id,
        'CUSTOMERS' as api_entity,
        'UPDATE_SAFETY_CENTER' as user_action_type,
        'USER_NO_ACCESS' as user_access_type,
        now() as created_at,
        now() as updated_at from atlas_bap_dashboard.role)
ON CONFLICT DO NOTHING;
