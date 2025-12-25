INSERT INTO atlas_bap_dashboard.access_matrix
        (id, role_id, api_entity, user_action_type, user_access_type, created_at, updated_at)
(SELECT atlas_bap_dashboard.uuid_generate_v4() as id,
        id as role_id,
        'FRFS' as api_entity,
        'LIST_FRFS_ROUTES' as user_action_type,
        'USER_FULL_ACCESS' as user_access_type,
        now() as created_at,
        now() as updated_at from atlas_bap_dashboard.role)
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix
        (id, role_id, api_entity, user_action_type, user_access_type, created_at, updated_at)
(SELECT atlas_bap_dashboard.uuid_generate_v4() as id,
        id as role_id,
        'FRFS' as api_entity,
        'ADD_FRFS_ROUTE' as user_action_type,
        'USER_FULL_ACCESS' as user_access_type,
        now() as created_at,
        now() as updated_at from atlas_bap_dashboard.role)
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix
        (id, role_id, api_entity, user_action_type, user_access_type, created_at, updated_at)
(SELECT atlas_bap_dashboard.uuid_generate_v4() as id,
        id as role_id,
        'FRFS' as api_entity,
        'DELETE_FRFS_ROUTE' as user_action_type,
        'USER_FULL_ACCESS' as user_access_type,
        now() as created_at,
        now() as updated_at from atlas_bap_dashboard.role)
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix
        (id, role_id, api_entity, user_action_type, user_access_type, created_at, updated_at)
(SELECT atlas_bap_dashboard.uuid_generate_v4() as id,
        id as role_id,
        'FRFS' as api_entity,
        'LIST_FRFS_ROUTE_FARE' as user_action_type,
        'USER_FULL_ACCESS' as user_access_type,
        now() as created_at,
        now() as updated_at from atlas_bap_dashboard.role)
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix
        (id, role_id, api_entity, user_action_type, user_access_type, created_at, updated_at)
(SELECT atlas_bap_dashboard.uuid_generate_v4() as id,
        id as role_id,
        'FRFS' as api_entity,
        'UPSERT_FRFS_ROUTE_FARE' as user_action_type,
        'USER_FULL_ACCESS' as user_access_type,
        now() as created_at,
        now() as updated_at from atlas_bap_dashboard.role)
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix
        (id, role_id, api_entity, user_action_type, user_access_type, created_at, updated_at)
(SELECT atlas_bap_dashboard.uuid_generate_v4() as id,
        id as role_id,
        'FRFS' as api_entity,
        'LIST_FRFS_STATION' as user_action_type,
        'USER_FULL_ACCESS' as user_access_type,
        now() as created_at,
        now() as updated_at from atlas_bap_dashboard.role)
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix
        (id, role_id, api_entity, user_action_type, user_access_type, created_at, updated_at)
(SELECT atlas_bap_dashboard.uuid_generate_v4() as id,
        id as role_id,
        'FRFS' as api_entity,
        'ADD_FRFS_STATION' as user_action_type,
        'USER_FULL_ACCESS' as user_access_type,
        now() as created_at,
        now() as updated_at from atlas_bap_dashboard.role)
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix
        (id, role_id, api_entity, user_action_type, user_access_type, created_at, updated_at)
(SELECT atlas_bap_dashboard.uuid_generate_v4() as id,
        id as role_id,
        'FRFS' as api_entity,
        'DELETE_FRFS_STATION' as user_action_type,
        'USER_FULL_ACCESS' as user_access_type,
        now() as created_at,
        now() as updated_at from atlas_bap_dashboard.role)
ON CONFLICT DO NOTHING;