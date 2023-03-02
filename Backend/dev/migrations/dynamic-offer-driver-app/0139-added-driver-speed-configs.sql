ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config
ADD COLUMN speed_normalizer DOUBLE PRECISION DEFAULT 28,
ADD COLUMN driver_speed_weightage INT DEFAULT 5,
ADD COLUMN location_update_sample_time INT DEFAULT 3,
ADD COLUMN min_location_updates INT DEFAULT 3,
ADD COLUMN default_driver_speed DOUBLE PRECISION DEFAULT 27.0;
