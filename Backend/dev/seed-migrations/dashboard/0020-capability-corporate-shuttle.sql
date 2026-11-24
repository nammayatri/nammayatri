-- Capabilities for the corporate shuttle endpoints in the control centre: reading the interest forms and routes
-- (RIDER_APP_MANAGEMENT/SHUTTLE_INTEREST/GET_*), and planning, editing and publishing the routes (POST_*_SHUTTLE_ROUTES_*).
--
-- The generator emits the capability_endpoint rows from the `migrate: capability:` lines in
-- spec/RiderPlatform/AppManagement/API/ShuttleInterest.yaml, but not the capability row itself -- it is seeded here.
INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('corporate-shuttle.interest.read', 'corporate-shuttle', 'View corporate shuttle interest forms and their map', false),
    ('corporate-shuttle.routes.write', 'corporate-shuttle', 'Plan, edit and publish corporate shuttle routes', false)
ON CONFLICT (id) DO NOTHING;

-- Granted to JUSPAY_ADMIN by role name, so it applies in every environment.
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.id FROM atlas_dashboard.role r
CROSS JOIN (VALUES ('corporate-shuttle.interest.read'), ('corporate-shuttle.routes.write')) AS c (id)
WHERE r.name = 'JUSPAY_ADMIN'
ON CONFLICT DO NOTHING;
