ALTER TABLE atlas_transporter.driver_information
  ADD COLUMN can_downgrade_to_hatchback boolean DEFAULT false NOT NULL;

ALTER TABLE atlas_transporter.driver_information
  ADD COLUMN can_downgrade_to_sedan boolean DEFAULT false NOT NULL;

UPDATE atlas_transporter.driver_information SET
  can_downgrade_to_hatchback = true,
  can_downgrade_to_sedan = true
  WHERE driver_id
  IN (
      '003093df-4f7c-440f-bada-4-suv_driver',
      '003093df-4f7c-440f-bada-sedan_driver',
      '003093df-4f7c-440f--hatchback_driver'
    );
