-- Capability rows for the RADAR ticket-desk endpoints
-- (PROVIDER_MANAGEMENT/RADAR_TICKETS/*).
--
-- The generator emits the capability_endpoint and role_capability links from the
-- `migrate: capability:` lines in RadarTickets.yaml, but not the capability rows
-- themselves -- those are seeded here, as in 0006, 0007 and 0009.
--
-- Without these rows, API_Management_RadarTickets.sql fails with
--   capability_endpoint_capability_id_fkey: Key (capability_id)=
--   (support.radar.read) is not present
-- which aborts the whole provider-dashboard migration transaction.

INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('support.radar.read', 'support', 'Read RADAR support tickets (Xyne desk)', false),
    ('support.radar.write', 'support', 'Create, reply and act on RADAR support tickets (Xyne desk)', false)
ON CONFLICT (id) DO NOTHING;
