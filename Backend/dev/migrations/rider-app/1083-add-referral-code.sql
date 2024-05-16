ALTER TABLE atlas_app.person ADD COLUMN has_taken_ride bool default false;
UPDATE atlas_app.person SET has_taken_ride = true WHERE id in (SELECT rider_id FROM atlas_app.booking WHERE status = 'COMPLETED');
ALTER TABLE atlas_app.person ALTER COLUMN has_taken_ride SET NOT NULL;
