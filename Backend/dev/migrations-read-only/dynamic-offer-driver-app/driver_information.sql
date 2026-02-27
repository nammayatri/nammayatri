CREATE TABLE atlas_driver_offer_bpp.driver_information ();

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN aadhaar_verified boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN active boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN admin_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN auto_pay_status text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN available_upi_apps text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN block_expiry_time timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN block_state_modifier text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN blocked boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN blocked_reason text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN can_downgrade_to_hatchback boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN can_downgrade_to_sedan boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN can_downgrade_to_taxi boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN can_switch_to_rental boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN comp_aadhaar_image_path text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN driver_dob timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN enabled boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN enabled_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN last_enabled_on timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN mode text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN num_of_locks integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN on_ride boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN payer_vpa text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN payment_pending boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN referral_code text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN referred_by_driver_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN subscribed boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN total_referred integer  default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN verified boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD PRIMARY KEY ( driver_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN air_condition_score double precision ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN selected_service_tiers text[] NOT NULL default '{}';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information DROP COLUMN selected_service_tiers;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ALTER COLUMN can_switch_to_rental DROP NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN can_switch_to_inter_city boolean default false;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN last_ac_status_checked_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN ac_usage_restriction_type text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN ac_restriction_lift_count integer NOT NULL default 0;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN has_advance_booking boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN toll_related_issue_count integer ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN forward_batching_enabled boolean ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN latest_scheduled_pickup text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN latest_scheduled_booking timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN latest_scheduled_pickup_lon double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN latest_scheduled_pickup_lat double precision ;

--- Drop columns section begins. Please be careful while running ---
ALTER TABLE atlas_driver_offer_bpp.driver_information DROP COLUMN latest_scheduled_pickup;
--- Drop columns section ends ---



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN payout_vpa text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN is_interoperable boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN payout_registration_order_id text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN payout_vpa_status text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN payout_vpa_bank_account text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN weekly_cancellation_rate_blocking_cooldown timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN daily_cancellation_rate_blocking_cooldown timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN block_reason_flag text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN payout_reg_amount_refunded double precision ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN preferred_secondary_special_loc_ids text[] ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN preferred_primary_special_loc_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN is_special_loc_warrior boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN driver_trip_end_location_lon double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN driver_trip_end_location_lat double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN extra_fare_mitigation_flag boolean ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN on_ride_trip_category text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN issue_breach_cooldown_times json ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN has_ride_started boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN soft_block_stiers text[] ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN soft_block_reason_flag text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN soft_block_expiry_time timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN special_loc_warrior_enabled_at timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN is_blocked_for_referral_payout boolean ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN onboarding_vehicle_category text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN services_enabled_for_subscription text[] ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN can_switch_to_intra_city boolean  default true;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN referred_by_operator_id text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN referred_by_fleet_owner_id text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN drunk_and_drive_violation_count integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN is_pet_mode_enabled boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN pan_number_hash text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN pan_number_encrypted text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN dl_number_hash text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN dl_number_encrypted text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN aadhaar_number_hash text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN aadhaar_number_encrypted text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN prepaid_subscription_balance double precision ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN plan_expiry_date timestamp with time zone ;

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN driver_flow_status text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN wallet_balance double precision ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN online_duration_refreshed_at timestamp with time zone  default CURRENT_TIMESTAMP;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN trip_distance_min_threshold integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN trip_distance_max_threshold integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN ride_request_volume integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN max_pickup_radius integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN is_silent_mode_enabled boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN is_tts_enabled boolean default true;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ALTER COLUMN is_tts_enabled DROP DEFAULT;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN is_high_accuracy_location_enabled boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN ride_request_volume_enabled boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN last_offline_time timestamp with time zone;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN rule_based_upgrade_tiers json ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN weekly_extra_kms integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN daily_extra_kms integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN onboarding_as character varying (255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN toll_route_blocked_till timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN approved boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN address_document_type text ;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN address text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN driver_bank_account_details text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ALTER COLUMN driver_bank_account_details TYPE json;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN tds_rate double precision ;