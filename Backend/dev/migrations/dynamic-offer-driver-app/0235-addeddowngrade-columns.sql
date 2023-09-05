ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN can_downgrade_to_sedan BOOLEAN  NOT NULL  DEFAULT false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN can_downgrade_to_hatchback BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN can_downgrade_to_taxi BOOLEAN NOT NULL DEFAULT false ;