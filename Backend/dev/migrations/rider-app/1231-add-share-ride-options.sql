ALTER TABLE atlas_app.person ADD COLUMN share_trip_with_emergency_contact_option text;

-- Query for backfilling the new column
-- UPDATE atlas_app.person p set share_trip_with_emergency_contact_option = 'SHARE_WITH_TIME_CONSTRAINTS' where p.share_trip_with_emergency_contacts = true;