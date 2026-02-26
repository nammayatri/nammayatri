CREATE TABLE atlas_driver_offer_bpp.driver_pool_config ();

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN actual_distance_threshold integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN distance_based_batch_split text[] NOT NULL default Array ['BatchSplitByPickupDistance { batchSplitSize = 1, batchSplitDelay = 0 }', 'BatchSplitByPickupDistance { batchSplitSize = 1, batchSplitDelay = 4 }'];
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN driver_batch_size integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN driver_position_info_expiry integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN driver_quote_limit integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN driver_request_count_limit integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN driver_to_destination_distance_threshold integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN driver_to_destination_duration integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN max_driver_quotes_required integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN max_number_of_batches integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN max_parallel_search_requests integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN max_radius_of_search integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN min_radius_of_search integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN pool_sorting_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN radius_shrink_value_for_drivers_on_ride integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN radius_step_size integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN schedule_try_times integer[] NOT NULL default '{1800, 900, 300}';
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN single_batch_process_time integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN threshold_to_ignore_actual_distance_threshold integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN trip_category text NOT NULL default 'All';
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN trip_distance integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN vehicle_variant text ;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN area text NOT NULL default 'Default';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN on_ride_radius_config text[] NOT NULL default ARRAY[]::TEXT[];
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN on_ride_batch_split_config text[] NOT NULL default Array ['BatchSplitByPickupDistance { batchSplitSize = 1, batchSplitDelay = 0 }', 'BatchSplitByPickupDistance { batchSplitSize = 1, batchSplitDelay = 4 }'];
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN max_parallel_search_requests_on_ride integer NOT NULL default 1;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN enable_forward_batching boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN current_ride_trip_category_valid_for_forward_batching text[] NOT NULL default Array ['OneWay_OneWayOnDemandDynamicOffer'];
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN batch_size_on_ride integer NOT NULL default 10;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN actual_distance_threshold_on_ride integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN time_bounds Text NOT NULL default 'Unbounded';
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ALTER COLUMN on_ride_batch_split_config SET DEFAULT Array ['BatchSplitByPickupDistanceOnRide { batchSplitSize = 1, batchSplitDelay = 0 }', 'BatchSplitByPickupDistanceOnRide { batchSplitSize = 1, batchSplitDelay = 4 }'];



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN distance_unit character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN use_one_to_one_osrm_mapping boolean ;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN batch_size_on_ride_with_straight_line_distance integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN enable_unified_pooling boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN dynamic_batch_size integer[];



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN self_request_if_rider_is_driver boolean NOT NULL default false;