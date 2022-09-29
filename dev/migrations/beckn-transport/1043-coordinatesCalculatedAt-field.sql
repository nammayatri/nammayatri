ALTER TABLE atlas_transporter.driver_location RENAME COLUMN updated_at to coordinates_calculated_at;
ALTER TABLE atlas_transporter.driver_location ALTER COLUMN coordinates_calculated_at DROP DEFAULT;
ALTER TABLE atlas_transporter.driver_location ADD COLUMN updated_at timestamp with time zone NOT NULL DEFAULT now();
