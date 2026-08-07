-- Grant JUSPAY_OPS and JUSPAY_ADMIN roles access to /bap/{merchantId}/person/bulkCreate.
-- Endpoint gate is DashboardAuth 'DASHBOARD_USER at the type level; this row seeds the
-- fine-grained AccessMatrix check that verifyAccessLevel does at handler entry.
-- ON CONFLICT DO NOTHING because migrations re-run on every deploy.
INSERT INTO atlas_bap_dashboard.access_matrix
  (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at)
SELECT
  atlas_bap_dashboard.uuid_generate_v4(),
  id,
  'DSL',
  'USER_FULL_ACCESS',
  'DASHBOARD_USER_BULK_CREATE',
  now(),
  now()
FROM atlas_bap_dashboard.role
WHERE name IN ('JUSPAY_OPS', 'JUSPAY_ADMIN')
ON CONFLICT DO NOTHING;
