ALTER TABLE atlas_app.person DROP COLUMN trigger_support;
ALTER TABLE atlas_app.person ADD COLUMN share_trip_with_emergency_contacts boolean default false;
ALTER TABLE atlas_app.person ADD COLUMN has_completed_mock_safety_drill boolean default false;
