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




------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN suburban_booking_allowed boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN straight_line_threshold integer  default 300;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN blocked_until_in_mins int  default 1440;