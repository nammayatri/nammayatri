INSERT INTO atlas_bpp_dashboard.access_matrix
        (id, role_id, api_entity, user_action_type, user_access_type, created_at, updated_at)
(SELECT atlas_bpp_dashboard.uuid_generate_v4() as id,
        id as role_id,
        'DRIVERS' as api_entity,
        'REMOVE_AC_USAGE_RESTRICTION' as user_action_type,
        'USER_NO_ACCESS' as user_access_type,
        now() as created_at,
        now() as updated_at from atlas_bpp_dashboard.role)
ON CONFLICT DO NOTHING;

-- Giving access to Admin
update atlas_bpp_dashboard.access_matrix set user_access_type = 'USER_FULL_ACCESS' where user_action_type = 'REMOVE_AC_USAGE_RESTRICTION' and role_id = '37947162-3b5d-4ed6-bcac-08841be1534d';

INSERT INTO atlas_bpp_dashboard.access_matrix
        (id, role_id, api_entity, user_action_type, user_access_type, created_at, updated_at)
(SELECT atlas_bpp_dashboard.uuid_generate_v4() as id,
        id as role_id,
        'MERCHANT' as api_entity,
        'UPSERT_FARE_POLICY' as user_action_type,
        'USER_NO_ACCESS' as user_access_type,
        now() as created_at,
        now() as updated_at from atlas_bpp_dashboard.role)
ON CONFLICT DO NOTHING;

-- Giving access to Admin
update atlas_bpp_dashboard.access_matrix set user_access_type = 'USER_FULL_ACCESS' where user_action_type = 'UPSERT_FARE_POLICY' and role_id = '37947162-3b5d-4ed6-bcac-08841be1534d';