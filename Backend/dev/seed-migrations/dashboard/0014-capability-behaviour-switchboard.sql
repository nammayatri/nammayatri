-- Behaviour switchboard: enable/disable/status of behaviour-engine packs per city.
-- Endpoint → capability mappings are generated from the NammaTag spec
-- (migrate: capability) in dev/migrations-read-only/dashboard/API_Management_NammaTag.sql;
-- this seed only creates the capabilities themselves.
INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('city-operations.behaviour.read', 'city-operations', 'View behaviour-engine enablement status per city', false),
    ('city-operations.behaviour.write', 'city-operations', 'Enable / disable behaviour-engine packs in a city', false)
ON CONFLICT (id) DO NOTHING;
