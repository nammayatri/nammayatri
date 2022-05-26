ALTER TABLE atlas_transporter.driver_information ADD COLUMN IF NOT EXISTS opt_for_rental BOOLEAN NOT NULL DEFAULT FALSE;
