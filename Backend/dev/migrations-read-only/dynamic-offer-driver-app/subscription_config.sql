CREATE TABLE atlas_driver_offer_bpp.subscription_config ();

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN allow_driver_fee_calc_schedule boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN allow_due_addition boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN allow_manual_payment_links boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN deep_link_expiry_time_in_minutes integer ;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN generic_batch_size_for_jobs integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN generic_job_reschedule_time integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN is_triggered_at_end_ride boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN max_retry_count integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN payment_link_channel text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN payment_link_job_time integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN payment_service_name text NOT NULL default 'Payment_Juspay';
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN send_deep_link boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN send_in_app_fcm_notifications boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN service_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN use_overlay_service boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD PRIMARY KEY ( service_name, merchant_operating_city_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN sgst_percentage_one_time_security_deposit double precision ;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN cgst_percentage_one_time_security_deposit double precision ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN payout_service_name text  default 'Payout_Juspay';
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN default_city_vehicle_category text ;




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN subscription_enabled_for_vehicle_categories text[] ;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN number_of_free_trial_rides integer ;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN free_trial_rides_applicable boolean ;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN execution_enabled_for_vehicle_categories text[] ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN is_subscription_enabled_at_category_level boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN enable_city_based_fee_switch boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN is_vendor_split_enabled boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN partial_due_clearance_message_key text ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN subscription_down boolean DEFAULT false;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ALTER COLUMN subscription_down SET DEFAULT false;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ALTER COLUMN subscription_down TYPE boolean;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN webhook_config json ;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN is_ui_enabled boolean ;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN ext_webhook_configs json ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN events_enabled_for_webhook text[] ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN is_free_trial_days_applicable boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN enable_service_usage_charge_default boolean ;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN data_entity_to_send text[] ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN generic_next_job_schedule_time_threshold integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN waive_off_offer_title text ;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN waive_off_offer_description text ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN autopay_enabled boolean  default true;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN show_manual_plans_in_ui boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN disabled_variants_for_subscription text[] ;