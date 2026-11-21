-- Capability for the corporate shuttle interest endpoints in the control centre
-- (RIDER_APP_MANAGEMENT/SHUTTLE_INTEREST/GET_SHUTTLE_INTEREST_LIST and GET_SHUTTLE_INTEREST_MAP_POINTS).
--
-- The generator emits the capability_endpoint rows from the `migrate: capability:` lines in
-- spec/RiderPlatform/AppManagement/API/ShuttleInterest.yaml, but not the capability row itself -- it is seeded here.
INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('corporate-shuttle.interest.read', 'corporate-shuttle', 'View corporate shuttle interest forms and their map', false)
ON CONFLICT (id) DO NOTHING;

-- Granted to JUSPAY_ADMIN by role name, so it applies in every environment.
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, 'corporate-shuttle.interest.read' FROM atlas_dashboard.role r
WHERE r.name = 'JUSPAY_ADMIN'
ON CONFLICT DO NOTHING;
