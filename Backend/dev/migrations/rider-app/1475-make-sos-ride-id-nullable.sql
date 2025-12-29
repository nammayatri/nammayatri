-- Make ride_id nullable in sos table to support non-ride SOS
ALTER TABLE atlas_app.sos ALTER COLUMN ride_id DROP NOT NULL;

