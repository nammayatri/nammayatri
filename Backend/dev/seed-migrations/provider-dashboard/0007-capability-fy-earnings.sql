-- Capability row for the FY/quarter earnings endpoint
-- (PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_FY_EARNINGS).
--
-- The generator emits the capability_endpoint and role_capability links from the
-- `migrate: capability:` line in the API spec, but not the capability row itself --
-- that is seeded here, as in 0006.

INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('finance.earnings.read', 'finance', '', false)
ON CONFLICT (id) DO NOTHING;
