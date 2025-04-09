CREATE TABLE atlas_driver_offer_bpp.transporter_config ();

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN aadhaar_image_resize_config json ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN aadhaar_verification_required boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN ac_status_check_gap integer NOT NULL default 7;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN actual_ride_distance_diff_threshold double precision NOT NULL default 1200;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN actual_ride_distance_diff_threshold_if_within_pickup_drop double precision NOT NULL default 2500;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN allow_default_plan_allocation boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN approx_ride_distance_diff_threshold double precision NOT NULL default 1200;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN arrived_pickup_threshold double precision  default 50;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN arrived_stop_threshold double precision  default 900;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN automatic_rc_activation_cut_off integer NOT NULL default 432000;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN avg_speed_of_vehicle json ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN bad_debt_batch_size integer NOT NULL default 1000;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN bad_debt_reschedule_time bigint NOT NULL default 60;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN bad_debt_scheduler_time bigint NOT NULL default 2592000;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN bad_debt_time_threshold bigint NOT NULL default 45;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN bank_error_expiry bigint NOT NULL default 3600;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN book_any_vehicle_downgrade_level integer NOT NULL default -1;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN cache_offer_list_by_driver_id boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN can_add_cancellation_fee boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN can_downgrade_to_hatchback boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN can_downgrade_to_sedan boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN can_downgrade_to_taxi boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN can_suv_downgrade_to_hatchback boolean  default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN can_suv_downgrade_to_taxi boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN can_switch_to_inter_city boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN can_switch_to_rental boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN cancellation_dist_diff integer NOT NULL default 50;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN cancellation_fee double precision NOT NULL default 10;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN cancellation_fee_dispute_limit integer NOT NULL default 3;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN cancellation_time_diff integer NOT NULL default 120;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN check_image_extraction_for_dashboard boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN coin_conversion_rate double precision NOT NULL default 0.4;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN coin_expire_time integer NOT NULL default 1296000;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN coin_feature boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN consider_drivers_for_search boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN consider_special_zone_ride_charges_in_free_trial boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN consider_special_zone_rides_for_plan_charges boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN cross_travel_cities text[] NOT NULL default '{}';
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN default_popup_delay integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN dl_number_verification boolean ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_auto_pay_execution_time bigint NOT NULL default 104400;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_auto_pay_execution_time_fall_back integer NOT NULL default 93600;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_auto_pay_notification_time bigint NOT NULL default 32400;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_distance_to_pickup_threshold_on_cancel integer NOT NULL default 50;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_distance_travelled_on_pickup_threshold_on_cancel bigint NOT NULL default 500;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_fee_calculation_time bigint ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_fee_calculator_batch_gap bigint ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_fee_calculator_batch_size int ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_fee_mandate_execution_batch_size integer NOT NULL default 20;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_fee_mandate_notification_batch_size integer NOT NULL default 20;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_fee_overlay_sending_time_limit_in_days integer NOT NULL default 15;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_fee_retry_threshold_config integer NOT NULL default 3;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_location_accuracy_buffer integer NOT NULL default 10;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_payment_cycle_buffer int NOT NULL default 14400;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_payment_cycle_duration int NOT NULL default 86400;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_payment_cycle_start_time int NOT NULL default 36000;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_payment_reminder_interval int NOT NULL default 1800;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_sms_receiving_limit json ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_time_spent_on_pickup_threshold_on_cancel integer NOT NULL default 600;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN drop_loc_threshold bigint NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN dummy_from_location json ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN dummy_to_location json ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN edit_loc_driver_permission_needed boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN edit_loc_time_threshold integer NOT NULL default 120;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN email_otp_config text ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN enable_dashboard_sms boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN enable_face_verification boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN enable_udf_for_offers boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN fake_otp_emails text[] NOT NULL default '{}';
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN fake_otp_mobile_numbers text[] NOT NULL default '{}';
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN fcm_service_account text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN fcm_token_key_prefix text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN fcm_url text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN free_trial_days integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN include_driver_currently_on_ride boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN is_avoid_toll boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN is_plan_mandatory boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN kapture_disposition text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN kapture_queue text NOT NULL default '';
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN languages_to_be_translated text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN mandate_execution_reschedule_interval bigint NOT NULL default 60;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN mandate_notification_reschedule_interval bigint NOT NULL default 60;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN mandate_validity int NOT NULL default 5;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN media_file_size_upper_limit integer NOT NULL default 10000000;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN media_file_url_pattern text NOT NULL default 'http://localhost:8016/ui/<DOMAIN>/media?filePath=<FILE_PATH>';
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN min_location_accuracy double precision NOT NULL default 50;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN min_rides_for_cancellation_score integer ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN min_rides_to_unlist integer ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN night_safety_end_time integer NOT NULL default 21600;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN night_safety_route_deviation_threshold integer NOT NULL default 1000;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN night_safety_start_time integer NOT NULL default 75600;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN notification_retry_count_threshold integer NOT NULL default 3;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN notification_retry_eligible_error_codes text[] NOT NULL default ARRAY['UC1', 'UC2', 'UC5', 'NU'];
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN notification_retry_time_gap bigint NOT NULL default 900;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN num_of_cancellations_allowed integer NOT NULL default 3;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN onboarding_retry_time_in_hours integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN onboarding_try_limit integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN open_market_un_blocked boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN order_and_notification_status_check_fall_back_time integer NOT NULL default 43200;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN order_and_notification_status_check_time bigint NOT NULL default 63000;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN order_and_notification_status_check_time_limit bigint NOT NULL default 345600;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN overlay_batch_size integer NOT NULL default 50;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN past_days_ride_counter integer NOT NULL default 3;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN pickup_loc_threshold bigint NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN place_name_cache_expiry_days integer ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN popup_delay_to_add_as_penalty integer ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN rating_as_decimal boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN rc_limit integer NOT NULL default 3;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN recompute_if_pickup_drop_not_outside_of_threshold boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN referral_link_password text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN refill_vehicle_model boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN ride_time_estimated_threshold bigint NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN route_deviation_threshold integer NOT NULL default 50;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN schedule_ride_buffer_time integer NOT NULL default 1800;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN search_repeat_limit integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN snap_to_road_confidence_threshold double precision NOT NULL default 0.75;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN special_drivers text[] NOT NULL default '{}';
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN special_location_tags text[] NOT NULL default '{}';
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN special_zone_booking_otp_expiry integer NOT NULL default 60;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN step_function_to_convert_coins integer NOT NULL default 250;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN subscription boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN subscription_start_time timestamp with time zone NOT NULL default TIMESTAMP '2023-08-31 00:00:00';
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN threshold_cancellation_percentage_to_unlist integer ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN threshold_cancellation_score integer ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN time_diff_from_utc int NOT NULL default 19800;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN update_notification_status_batch_size integer NOT NULL default 20;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN update_order_status_batch_size integer NOT NULL default 20;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN upwards_recompute_buffer double precision NOT NULL default 2000;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN use_offer_list_cache boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN use_with_snap_to_road_fallback boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN variants_to_enable_for_subscription text[] NOT NULL default '{AUTO_RICKSHAW}';
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN volunteer_sms_sending_limit json ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD PRIMARY KEY ( merchant_operating_city_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN use_silent_fcm_for_forward_batch boolean NOT NULL default false;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN fare_recompute_weekly_extra_kms_threshold integer NOT NULL default 20000;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN fare_recompute_daily_extra_kms_threshold integer NOT NULL default 5000;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN to_notify_driver_for_extra_kms_limit_exceed boolean NOT NULL default True;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN arriving_pickup_threshold double precision NOT NULL default 100;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN distance_unit character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN payout_batch_limit integer NOT NULL default 10;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN enable_toll_crossed_notifications boolean NOT NULL default false;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN update_payout_status_batch_size integer NOT NULL default 20;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN last_ndays_to_check_for_payout_order_status integer NOT NULL default 2;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN min_ride_distance_threshold_for_referral_payout integer NOT NULL default 1500;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN min_pickup_distance_threshold_for_referral_payout integer NOT NULL default 500;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN min_ride_distance_threshold_for_referral_payout SET DEFAULT 1500.0;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN max_payout_referral_for_a_day integer NOT NULL default 5;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN scheduled_ride_job_reschedule_time integer  default 300;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN scheduled_ride_filter_exclusion_threshold_hours integer  default 2;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN minm_rental_and_scheduled_booking_lead_time_hours integer  default 24;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN grace_time_for_scheduled_ride_pickup integer  default 300;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN exotel_app_id_mapping json ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN create_document_required boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN schedule_payout_for_day integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN is_device_id_checks_required boolean ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN min_pickup_distance_threshold_for_referral_payout SET DEFAULT 10;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN dummy_show_driver_additions boolean ;
------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN exotel_status_check_scheduler_delay integer  default 120;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN cached_devices_os_for_search_request text[]  default '{}';



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN cancellation_rate_window integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN cancellation_rate_calculation_threshold integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN arrival_time_buffer_of_vehicle json ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN upwards_recompute_buffer_percentage integer ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN demand_hotspots_config json ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN dp_geo_hash_percision integer  default 5;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN scheduled_ride_search_repeat_limit integer  default 10;

--- Now DSL don't allow dropping tables instead we will drop not null constraint if any .Please be careful while running ---
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN min_ride_distance_threshold_for_referral_payout DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN min_pickup_distance_threshold_for_referral_payout DROP NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN dp_white_listed_geohash text[]  default ARRAY[]::TEXT[];
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN dp_black_listed_geohash text[]  default ARRAY[]::TEXT[];



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN weekly_offence_suspension_time_hours integer ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN weekly_min_rides_for_nudging integer ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN weekly_min_rides_for_blocking integer ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN daily_offence_suspension_time_hours integer ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN daily_min_rides_for_nudging integer ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN daily_min_rides_for_blocking integer ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN cancellation_rate_threshold_weekly integer ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN cancellation_rate_threshold_daily integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN weekly_condition_cooldown_time_hours integer  default 168;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN daily_condition_cooldown_time_hours integer  default 24;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_driven_search_req_expiry integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN otp_ride_start_restriction_radius integer ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN disable_list_scheduled_booking_api boolean  default false;

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN issue_breach_config json ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN recent_scheduled_bookings_safe_limit integer  default 50;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN min_distance_for_stop_fcm double precision  default 100;
------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN recompute_distance_thresholds json  default '[{"estimatedDistanceUpper": 5000,"minThresholdPercentage": 40,"minThresholdDistance": 1000},{"estimatedDistanceUpper": 10000,"minThresholdPercentage": 30,"minThresholdDistance": 1000},{"estimatedDistanceUpper": 15000,"minThresholdPercentage": 20,"minThresholdDistance": 1000},{"estimatedDistanceUpper": 9999999,"minThresholdPercentage": 10,"minThresholdDistance": 1000}]' :: json;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN min_threshold_for_pass_through_destination int ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN end_ride_distance_threshold double precision  default 100;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN fleet_alert_threshold integer ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN requires_onboarding_inspection boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN enable_overcharging_blocker boolean  default false;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN normal_ride_bulk_loc_update_batch_size integer ;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN meter_ride_bulk_loc_update_batch_size integer ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN dynamic_referral_code_valid_for_minutes integer ;

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN dynamic_referral_code_enabled boolean ;

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN allowed_referral_entities text[]  default '{DRIVER}';


ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN qar_cal_radius_in_km double precision ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN is_dynamic_pricing_qar_cal_enabled boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN generate_referral_code_for_operator boolean  default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN generate_referral_code_for_fleet boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN max_allowed_doc_size_in_mb integer ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN bulk_waive_off_limit integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN rc_expiry_checks boolean ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN require_route_mapping_in_vehicle boolean  default true;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN enable_existing_vehicle_in_bulk_upload boolean  default false;