ALTER TABLE atlas_transporter.driver_information
  ADD COLUMN can_downgrade_to_hatchack boolean DEFAULT false NOT NULL;

ALTER TABLE atlas_transporter.driver_information
  ADD COLUMN can_downgrade_to_sedan boolean DEFAULT false NOT NULL;
