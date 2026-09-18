-- Capability mapping for GET /bap/{merchantShortId}/{city}/person/list.
--
-- The route is DashboardAuth 'DASHBOARD_USER and the handler runs Capability.enforce, which fails
-- closed: without this row the endpoint is 403 for everyone, including admins.

INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id)
VALUES ('admin.user.read', 'DASHBOARD', 'DASHBOARD_USER_PT_LIST')
ON CONFLICT (capability_id, server_name, endpoint_id) DO NOTHING;
