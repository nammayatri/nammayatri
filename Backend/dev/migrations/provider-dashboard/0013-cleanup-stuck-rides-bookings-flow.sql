-- for testing purposes
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        'MULTIPLE_RIDE_END'
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'RIDES' AND T1.user_access_type = 'USER_FULL_ACCESS' AND T1.user_action_type = 'RIDE_END'
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        'MULTIPLE_RIDE_CANCEL'
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'RIDES' AND T1.user_access_type = 'USER_FULL_ACCESS' AND (T1.user_action_type = 'RIDE_CANCEL' OR T1.user_action_type = 'MULTIPLE_RIDE_CANCEL')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        'MULTIPLE_BOOKING_SYNC'
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'RIDES' AND T1.user_access_type = 'USER_FULL_ACCESS' AND (T1.user_action_type = 'RIDE_END' OR T1.user_action_type = 'MULTIPLE_RIDE_END')
    )
ON CONFLICT DO NOTHING;
