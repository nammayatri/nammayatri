-- Capability row for the fleet cash-ride update endpoint
-- (PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_CASH_RIDE_UPDATE).
--
-- The generator emits the capability_endpoint and role_capability links from the
-- `migrate: capability:` line in the API spec, but not the capability row itself --
-- that is seeded here, as in 0006 and 0007.
--
-- Without this row, API_Fleet_Driver.sql fails with
--   capability_endpoint_capability_id_fkey: Key (capability_id)=
--   (city-operations.driver_management.write) is not present
-- which aborts the whole provider-dashboard migration transaction.

INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('city-operations.driver_management.write', 'city-operations', '', false)
ON CONFLICT (id) DO NOTHING;
