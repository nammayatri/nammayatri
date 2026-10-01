-- Capability mapping for GET /bap/{merchantShortId}/{city}/person/list.
--
-- BAP-side counterpart of provider-dashboard/0008-pt-list-capability-endpoint.sql. API.Person is
-- mounted in both dashboards, and the route is DashboardAuth 'DASHBOARD_USER whose handler runs
-- Capability.enforce, which fails closed: without this row the endpoint is 403 for everyone,
-- including admins.

INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id)
VALUES ('admin.user.read', 'DASHBOARD', 'DASHBOARD_USER_PT_LIST')
ON CONFLICT (capability_id, server_name, endpoint_id) DO NOTHING;
