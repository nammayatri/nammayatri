INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('city-operations.pass.write', 'city-config', '', false)
ON CONFLICT (id) DO NOTHING;
