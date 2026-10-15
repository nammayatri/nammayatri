INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('system-config.kaal_chakra.read', 'system-config', 'View Kaal Chakra job run history and status', false),
    ('system-config.kaal_chakra.write', 'system-config', 'Reschedule or cancel Kaal Chakra scheduler jobs', false)
ON CONFLICT (id) DO NOTHING;
