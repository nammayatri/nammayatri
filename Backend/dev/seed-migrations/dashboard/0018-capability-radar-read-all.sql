-- Widens RADAR reads from the caller's own tickets to the whole desk.
-- Checked inside the RadarTickets handlers, not bound to any endpoint, so the
-- generator emits no capability_endpoint row for it. Granted per role or per
-- person from Access Control.
INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('support.radar.read_all', 'support', 'See every RADAR ticket, not only your own', false)
ON CONFLICT (id) DO NOTHING;
