ALTER TABLE atlas_driver_offer_bpp.transporter_config
ADD COLUMN driver_maximum_speed DOUBLE PRECISION DEFAULT 27,
ADD COLUMN speed_weightage_of_driver INT DEFAULT 5,
ADD COLUMN minimum_location_updates INT DEFAULT 3,
ADD COLUMN loc_updates_sample_time INT DEFAULT 3,
ADD COLUMN driver_default_speed INT DEFAULT 27;