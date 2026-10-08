-- During-ride feedback (PROVIDER_MANAGEMENT/RIDE_FEEDBACK/* on driver-app: ride preview, responses,
-- meta; RIDER_MANAGEMENT/RIDE_FEEDBACK/* on rider-app: retry actions). The questions themselves are a
-- Config Pilot table, edited through the NammaTag Config Pilot APIs and their capabilities.
-- The endpoint links come from migrations-read-only/dashboard/API_Management_RideFeedback.sql;
-- these are the capabilities they point to. Grant them to roles from the dashboard.
INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('system-config.ride_feedback.read', 'system-config', 'View during-ride feedback ride previews and responses', false),
    ('system-config.ride_feedback.write', 'system-config', 'Retry failed during-ride feedback actions', false)
ON CONFLICT (id) DO NOTHING;
