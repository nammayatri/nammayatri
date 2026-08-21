INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('finance.adjustment.read', 'finance', '', false)
ON CONFLICT (id) DO NOTHING;
