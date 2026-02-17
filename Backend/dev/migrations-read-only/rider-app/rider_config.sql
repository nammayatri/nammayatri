CREATE TABLE atlas_app.rider_config ();

ALTER TABLE atlas_app.rider_config ADD COLUMN app_url text NOT NULL default 'nammayatri.in/link/rider/rmxw';
ALTER TABLE atlas_app.rider_config ADD COLUMN enable_emergency_contact_added_message boolean NOT NULL default true;
ALTER TABLE atlas_app.rider_config ADD COLUMN enable_local_police_support boolean NOT NULL default false;
ALTER TABLE atlas_app.rider_config ADD COLUMN enable_support_for_safety boolean NOT NULL default false;
ALTER TABLE atlas_app.rider_config ADD COLUMN local_police_number text ;
ALTER TABLE atlas_app.rider_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.rider_config ADD COLUMN safety_check_end_time integer NOT NULL default 21600;
ALTER TABLE atlas_app.rider_config ADD COLUMN safety_check_start_time integer NOT NULL default 75600;
ALTER TABLE atlas_app.rider_config ADD COLUMN time_diff_from_utc integer NOT NULL default 19800;
ALTER TABLE atlas_app.rider_config ADD COLUMN tracking_short_url_pattern text NOT NULL default 'nammayatri.in/t/';
ALTER TABLE atlas_app.rider_config ADD COLUMN video_file_size_upper_limit integer NOT NULL default 15000000;
ALTER TABLE atlas_app.rider_config ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.rider_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.rider_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.rider_config ADD PRIMARY KEY ( merchant_operating_city_id);


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN collect_auto_complete_data boolean ;
ALTER TABLE atlas_app.rider_config ADD COLUMN special_zone_radius integer NOT NULL default 150;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN distance_weightage integer NOT NULL default 70;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN collect_mmi_route_data boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN is_avoid_toll boolean NOT NULL default true;
ALTER TABLE atlas_app.rider_config ADD COLUMN auto_unblock_safety_center_after_days integer NOT NULL default 14;



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN place_name_cache_expiry_days integer ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN booking_sync_status_call_seconds_diff_threshold integer ;



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN kapture_queue text NOT NULL default '';


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN email_otp_config json;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN email_magic_link_config json;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN kapture_config json NOT NULL default '{"kaptureQueue":"", "disposition":""}';


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN police_trigger_delay integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN ivr_trigger_delay integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN hard_limit_for_safety_jobs integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN exotel_app_id_mapping json ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN incident_report_support boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN settle_cancellation_fee_before_next_ride boolean ;

------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN exotel_status_check_scheduler_delay integer  default 120;



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN cx_agent_details text [] ;



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN cs_alert_trigger_delay integer ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN avg_speed_in_km_per_hr integer  default 20;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN sensitive_words text [] ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN payout_batch_size integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN payout_batch_delay integer ;

ALTER TABLE atlas_app.rider_config ADD COLUMN execute_payment_delay integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN cancellation_payment_delay integer ;



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN make_multi_modal_search boolean  default false;




------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN maximum_walk_distance integer  default 600;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN sensitive_words_for_exact_match text [] ;
------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN post_ride_safety_notification_delay integer  default 60;



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN driver_referred_search_req_expiry integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN dynamic_logic_update_password text ;



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN feedback_alert_rating_threshold integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN use_user_settings_for_safety_ivr boolean  default false;



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN payout_referral_program boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN payout_referral_start_date timestamp with time zone  default '2024-12-20 12:00:00.000000+00';


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN threshold_cancellation_percentage_to_block integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN min_rides_to_block integer ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN metro_booking_allowed boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN min_rides_to_show_cancellation_rate integer ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN auto_send_booking_details_via_whatsapp boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN permissible_modes text []  default '{Walk, Bus, MetroRail, Subway}';
ALTER TABLE atlas_app.rider_config ADD COLUMN minimum_walk_distance integer  default 100;
ALTER TABLE atlas_app.rider_config ADD COLUMN max_allowed_public_transport_legs integer  default 2;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN multimodal_testing boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN is_first_referred_ride_enabled boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN rentals_config text [] ;
ALTER TABLE atlas_app.rider_config ADD COLUMN intercity_search_locations text [] ;
ALTER TABLE atlas_app.rider_config ADD COLUMN excluded_vehicle_variants text [] ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN payout_referral_threshold_per_month integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN payout_referral_threshold_per_day integer ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN variant_list_for_near_by_req text [] ;
ALTER TABLE atlas_app.rider_config ADD COLUMN near_by_driver_ring_bucket_cfg json ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN nearby_driver_search_radius double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN is_device_id_check_disabled boolean ;



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN enable_bus_filtering boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN fare_cache_rentals_config json ;
ALTER TABLE atlas_app.rider_config ADD COLUMN fare_cache_inter_city_search_locations json ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN suburban_booking_allowed boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN straight_line_threshold integer  default 300;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN blocked_until_in_mins int  default 1440;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN ticket_asset_domain text ;

------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN qr_ticket_restriction_start_time time without time zone;
ALTER TABLE atlas_app.rider_config ADD COLUMN qr_ticket_restriction_end_time time without time zone;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN ticketing_permission_config json ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN user_service_tier_order_config json ;
ALTER TABLE atlas_app.rider_config ADD COLUMN ny_regular_subscription_batch_size integer ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN no_of_ride_requests_config integer ;



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN bus_tracking_config json  default '{"maxScoreDistanceInMeters":15.0,"goodScoreDistanceInMeters":30.0,"fairScoreDistanceInMeters":45.0,"maxScore":10.0,"goodScore":7.0,"fairScore":4.0,"thresholdFactor":0.5,"thresholdSeconds":30.0,"movementThresholdInMeters":25.0}';


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN pickup_instructions_threshold integer ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN enable_multi_modal_for_all_users boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN pickup_instructions_proximity_meters double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ALTER COLUMN pickup_instructions_proximity_meters TYPE integer;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN initiate_first_multimodal_journey boolean  default false;



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN ny_regular_execution_time_offset_minutes integer ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN ny_regular_min_gap_seconds integer ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN enable_auto_journey_refund boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN filter_walk_and_unspecified_transit_modes boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN ny_regular_master_job_next_run_offset_seconds integer ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN enable_igm_issue_flow boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN refund_buffer_ttl_sec integer ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN refund_status_update_retries integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN refund_status_update_interval integer ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN boost_search_pre_selection_service_tier_config json ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN bus_booking_allowed boolean ;
ALTER TABLE atlas_app.rider_config ADD COLUMN domain_route_calculation_enabled_modes text []  default '{Bus, Subway}';



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN upcoming_bus_threshold_sec integer  default 3600;
ALTER TABLE atlas_app.rider_config ADD COLUMN bus_tier_sorting_config json ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN source_of_service_tier text default 'NANDI';


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN track_vehicle_key_expiry integer  default 900;
ALTER TABLE atlas_app.rider_config ADD COLUMN distance_to_nearest_stop_threshold integer  default 200;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN bus_scan_route_calculation_enabled_modes boolean  default true;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN validate_set_onboarding_vehicle_request boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN service_tier_relationship_cfg json ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN subway_transit_types text []  default '{FIRST_CLASS,SECOND_CLASS}';
ALTER TABLE atlas_app.rider_config ADD COLUMN bus_transit_types text []  default '{ORDINARY,NON_AC,SPECIAL,EXECUTIVE,EXPRESS,AC}';


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN journey_options_sorting_type text ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN update_ticket_validity_in_seconds_post_set_onboarding integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN domain_public_transport_data_version integer ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN enable_ride_end_offers boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN metrics_blacklist_patterns text [] ;
ALTER TABLE atlas_app.rider_config ADD COLUMN dashboard_media_file_url_pattern text ;



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN nearby_bus_search_radius double precision ;
ALTER TABLE atlas_app.rider_config ADD COLUMN max_nearby_buses integer ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN enable_online_payment_ride boolean  default false;


------- SQL updates -------



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN nearby_bus_max_time_threshold integer ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN special_vehicle_notification_configs json ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN offer_list_cache_version text ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN email_business_verification_config json ;



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN frfs_metrics_api_key text ;



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN subway_restriction_start_time time without time zone ;
ALTER TABLE atlas_app.rider_config ADD COLUMN subway_restriction_end_time time without time zone ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN nearby_bus_radius_bucket_step integer ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN single_mode_walk_speed double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN default_service_tier_order_config text [] ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN suburban_fare_caching_allowed boolean ;
ALTER TABLE atlas_app.rider_config ADD COLUMN pt_circuit_breaker_config json ;
ALTER TABLE atlas_app.rider_config ADD COLUMN metro_fare_caching_allowed boolean ;
ALTER TABLE atlas_app.rider_config ADD COLUMN bus_fare_caching_allowed boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN metro_ticket_allowed boolean;
ALTER TABLE atlas_app.rider_config ADD COLUMN suburban_ticket_allowed boolean;
ALTER TABLE atlas_app.rider_config ADD COLUMN bus_ticket_allowed boolean;




------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN kafka_topic_name text  default 'gps-data';


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN weekly_offence_suspension_time_hours integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN weekly_min_rides_for_nudging integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN weekly_min_rides_for_blocking integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN weekly_condition_cooldown_time_hours integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN monthly_offence_suspension_time_hours integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN monthly_min_rides_for_nudging integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN monthly_min_rides_for_blocking integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN monthly_condition_cooldown_time_hours integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN daily_offence_suspension_time_hours integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN daily_min_rides_for_nudging integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN daily_min_rides_for_blocking integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN daily_condition_cooldown_time_hours integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN cancellation_rate_window integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN cancellation_rate_threshold_weekly integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN cancellation_rate_threshold_monthly integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN cancellation_rate_threshold_daily integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN cancellation_rate_calculation_threshold integer ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN enable_customer_cancellation_rate_blocking boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN static_customer_id_threshold_day date ;



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN invoice_logo_url text ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN external_sos_config json;




------- SQL updates -------


------- SQL updates -------




------- SQL updates -------

