CREATE TABLE atlas_driver_offer_bpp.driver_pool_config ();

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN actual_distance_threshold text ;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN distance_based_batch_split text[] NOT NULL default Array ['BatchSplitByPickupDistance { batchSplitSize = 1, batchSplitDelay = 0 }', 'BatchSplitByPickupDistance { batchSplitSize = 1, batchSplitDelay = 4 }'];
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN driver_batch_size integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN driver_position_info_expiry text ;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN driver_quote_limit integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN driver_request_count_limit integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN driver_to_destination_distance_threshold text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN driver_to_destination_duration text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN max_driver_quotes_required integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN max_number_of_batches integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN max_parallel_search_requests integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN max_radius_of_search text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN min_radius_of_search text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN pool_sorting_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN radius_shrink_value_for_drivers_on_ride text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN radius_step_size text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN single_batch_process_time text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN trip_distance text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN vehicle_variant text ;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ALTER COLUMN vehicle_variant SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ALTER COLUMN vehicle_variant SET DEFAULT 'All';
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN trip_category text NOT NULL default 'All';
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config DROP CONSTRAINT driver_pool_config_pkey;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config DROP COLUMN id;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD PRIMARY KEY ( merchant_operating_city_id, trip_category, trip_distance, vehicle_variant);