-- Delete old Exophone access entries (in case of duplicates or incorrect format)
DELETE FROM atlas_bpp_dashboard.access_matrix
WHERE user_action_type LIKE '%EXOPHONE%';

-- Add Exophone API access (DSL-based) to all roles that have DRIVERS access
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        'DSL',
        'USER_FULL_ACCESS',
        unnest (ARRAY [
            'PROVIDER_MANAGEMENT/EXOPHONE/POST_EXOPHONE_CREATE',
            'PROVIDER_MANAGEMENT/EXOPHONE/GET_EXOPHONE_LIST',
            'PROVIDER_MANAGEMENT/EXOPHONE/GET_EXOPHONE',
            'PROVIDER_MANAGEMENT/EXOPHONE/POST_EXOPHONE_UPDATE',
            'PROVIDER_MANAGEMENT/EXOPHONE/DELETE_EXOPHONE_DELETE'
        ])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'DRIVERS' AND T1.user_access_type = 'USER_FULL_ACCESS'
	  GROUP BY role_id
    )
ON CONFLICT DO NOTHING;
