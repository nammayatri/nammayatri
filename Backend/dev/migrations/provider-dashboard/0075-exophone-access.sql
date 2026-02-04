-- Delete old Exophone access entries (in case of duplicates or incorrect format)
DELETE FROM atlas_bpp_dashboard.access_matrix
WHERE user_action_type LIKE '%EXOPHONE%';

-- Add Exophone API access (DSL-based) to all roles that have DRIVERS access
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        r.role_id,
        'DSL',
        'USER_FULL_ACCESS',
        action
    FROM
      ( SELECT DISTINCT role_id
        FROM atlas_bpp_dashboard.access_matrix
        WHERE api_entity = 'DRIVERS' AND user_access_type = 'USER_FULL_ACCESS'
      ) AS r
    CROSS JOIN unnest (ARRAY [
            'PROVIDER_MANAGEMENT/EXOPHONE/POST_EXOPHONE_CREATE',
            'PROVIDER_MANAGEMENT/EXOPHONE/GET_EXOPHONE_LIST',
            'PROVIDER_MANAGEMENT/EXOPHONE/GET_EXOPHONE',
            'PROVIDER_MANAGEMENT/EXOPHONE/POST_EXOPHONE_UPDATE',
            'PROVIDER_MANAGEMENT/EXOPHONE/DELETE_EXOPHONE_DELETE'
    ]) AS action
    )
ON CONFLICT (role_id, api_entity, user_action_type) DO NOTHING;
