-- Capabilities and roles for the permit dashboard APIs
-- (PROVIDER_PERMIT/PERMIT/...). The generator emits the capability_endpoint rows from the
-- `migrate: capability:` lines in lib/permit's Permit.yaml, but not the capability rows
-- themselves, so they are seeded here.
--
-- Only PICNIC_DC holds permit.application.dc_decide: the District Commissioner alone
-- decides 60 to 90 minute picnic trips.
--
-- A capability applies to an endpoint, not to a permit type. These roles are named for
-- picnic permits but are not limited to them: decide per-type access before a second
-- permit type is enabled.

INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('permit.application.read', 'permit', 'View permit applications and their photos', false),
    ('permit.application.decide', 'permit', 'Approve or reject standard permit applications', false),
    ('permit.application.dc_decide', 'permit', 'Approve or reject applications reserved for the District Commissioner', false),
    ('permit.pass.verify', 'permit', 'Scan and verify permit passes', false)
ON CONFLICT (id) DO NOTHING;

INSERT INTO atlas_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at)
SELECT 'b25e43a5-90cb-59da-95c7-5027214d9574', 'PICNIC_REVIEWER', 'DASHBOARD_USER', 'PICNIC_REVIEWER (created by capability seed)', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.role WHERE name = 'PICNIC_REVIEWER');

INSERT INTO atlas_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at)
SELECT 'fa232d39-0a73-562d-aade-eccf159f6c1e', 'PICNIC_DC', 'DASHBOARD_USER', 'PICNIC_DC (created by capability seed)', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.role WHERE name = 'PICNIC_DC');

INSERT INTO atlas_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at)
SELECT 'b99e8432-7531-54e8-99a9-bd2ed3a2b671', 'PICNIC_CHECKPOINT', 'DASHBOARD_USER', 'PICNIC_CHECKPOINT (created by capability seed)', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.role WHERE name = 'PICNIC_CHECKPOINT');

-- Granted by role name, so it applies in every environment.
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.capability_id
FROM atlas_dashboard.role r
JOIN (VALUES
    ('PICNIC_REVIEWER', 'permit.application.read'),
    ('PICNIC_REVIEWER', 'permit.application.decide'),
    ('PICNIC_DC', 'permit.application.read'),
    ('PICNIC_DC', 'permit.application.decide'),
    ('PICNIC_DC', 'permit.application.dc_decide'),
    ('PICNIC_CHECKPOINT', 'permit.pass.verify')
) AS c(role_name, capability_id) ON c.role_name = r.name
ON CONFLICT DO NOTHING;
