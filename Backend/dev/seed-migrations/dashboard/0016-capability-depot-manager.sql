-- Capability rows for the depot-manager endpoints
-- (PROVIDER_APP_MANAGEMENT/DEPOT_MANAGER/{POST_DEPOT_MANAGER_UPSERT_ONE,
--  POST_DEPOT_MANAGER_UPSERT_MANY, DELETE_DEPOT_MANAGER, GET_DEPOT_MANAGER_LIST,
--  GET_DEPOT_MANAGER_BY_MOBILE_NUMBER, GET_DEPOT_MANAGER_BY_DEPOT_CODE}).
--
-- The generator emits the capability_endpoint and role_capability links from the
-- `migrate: capability:` line in the API spec, but not the capability rows themselves --
-- those are seeded here, as in 0006, 0007, 0009 and 0011.
--
-- Without these rows, API_AppManagement_DepotManager.sql fails with
--   capability_endpoint_capability_id_fkey: Key (capability_id)=
--   (transit-operations.depot-manager.write) is not present
-- which aborts the whole dashboard migration transaction.

INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('transit-operations.depot-manager.read',  'transit-operations', '', false),
    ('transit-operations.depot-manager.write', 'transit-operations', '', false)
ON CONFLICT (id) DO NOTHING;
