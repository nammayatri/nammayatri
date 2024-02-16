ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN actual_pickup_distance_weightage INT NOT NULL DEFAULT 0;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN keep_hidden_for_seconds int NOT NULL default 0;
