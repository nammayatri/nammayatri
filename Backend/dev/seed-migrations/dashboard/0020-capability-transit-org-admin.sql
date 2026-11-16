INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('transit-operations.org.admin', 'transit-operations', 'Manage transit organizations and assign dashboard users to them', false)
ON CONFLICT (id) DO NOTHING;
