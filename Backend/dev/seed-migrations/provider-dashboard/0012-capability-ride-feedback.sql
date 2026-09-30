-- Capability rows for the during-ride feedback management endpoints
-- (RIDER_MANAGEMENT/RIDE_FEEDBACK/*: config list/get/create/update/toggle/clone/validate,
--  ride preview, ride responses, retry actions, meta).
--
-- The generator emits the capability_endpoint links from the `migrate: capability:` lines in
-- spec/RiderPlatform/Management/API/RideFeedback.yaml, but not the capability rows themselves.
-- Without these rows API_Management_RideFeedback.sql fails on capability_endpoint_capability_id_fkey.
-- Grant the capabilities to roles from the dashboard after they exist.

INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('system-config.ride_feedback.read',  'system-config', 'View during-ride feedback questions, previews and responses', false),
    ('system-config.ride_feedback.write', 'system-config', 'Create, edit, enable and clone during-ride feedback questions', false)
ON CONFLICT (id) DO NOTHING;
