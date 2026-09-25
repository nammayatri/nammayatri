INSERT INTO atlas_bap_dashboard.capability (id, domain, description, is_system) VALUES
    ('system-config.dynamic_logic.write_without_password', 'system-config', 'Update app dynamic logic rules without the per-city update password', false)
ON CONFLICT DO NOTHING;


--   INSERT INTO atlas_bap_dashboard.role_capability (role_id, capability_id)
--   SELECT r.id, 'system-config.dynamic_logic.write_without_password'
--     FROM atlas_bap_dashboard.role r WHERE r.name = '<ROLE_NAME>'
--   ON CONFLICT DO NOTHING;
