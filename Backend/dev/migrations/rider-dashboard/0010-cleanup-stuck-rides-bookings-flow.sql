-- for testing purposes
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        'MULTIPLE_RIDE_SYNC'
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'RIDES' AND T1.user_access_type = 'USER_FULL_ACCESS' AND T1.user_action_type = 'RIDE_FORCE_SYNC'
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        'MULTIPLE_BOOKING_SYNC'
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'RIDES' AND T1.user_access_type = 'USER_FULL_ACCESS' AND T1.user_action_type = 'RIDE_FORCE_SYNC'
    )
ON CONFLICT DO NOTHING;

DELETE FROM atlas_bap_dashboard.access_matrix
    WHERE user_action_type = 'RIDE_FORCE_SYNC';
