ALTER TABLE atlas_app.person ADD COLUMN has_taken_ride bool default false;
ALTER TABLE atlas_app.person ALTER COLUMN has_taken_ride SET NOT NULL;
