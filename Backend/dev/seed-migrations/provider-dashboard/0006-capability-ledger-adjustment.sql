INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('finance.adjustment.read', 'finance', '', false),
    ('finance.adjustment.approve', 'finance', '', false),
    ('city-config.cancel.read', 'city-config', '', false),
    ('city-config.cancel.write', 'city-config', '', false)
ON CONFLICT (id) DO NOTHING;
