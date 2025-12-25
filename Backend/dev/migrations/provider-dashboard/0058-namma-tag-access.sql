-- required only for local testing

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        'NAMMA_TAG',
        'USER_FULL_ACCESS',
        unnest (ARRAY ['CREATE_CHAKRA_QUERY', 'CREATE_NAMMA_TAG', 'RUN_KAAL_CHAKRA_JOB'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'DRIVERS' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_READ_ACCESS')
	  GROUP BY role_id
    )
ON CONFLICT DO NOTHING;
