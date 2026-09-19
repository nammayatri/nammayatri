-- Capability rows for the dynamic-trip-ETA endpoints
-- (RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_*_ETA_*).
--
-- The generator emits the capability_endpoint and role_capability links from the
-- `migrate: capability:` line in the API spec, but not the capability rows themselves --
-- those are seeded here, as in 0006/0007. capability_endpoint.capability_id FKs to
-- capability.id, so this must run before API_AppManagement_TransitOperator.sql.

INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('transit-config.eta_variant.read', 'transit-config', '', false),
    ('transit-config.eta_variant.write', 'transit-config', '', false),
    ('transit-config.station_eta.write', 'transit-config', '', false),
    ('transit-operations.eta_override.read', 'transit-operations', '', false),
    ('transit-operations.eta_override.write', 'transit-operations', '', false)
ON CONFLICT (id) DO NOTHING;
