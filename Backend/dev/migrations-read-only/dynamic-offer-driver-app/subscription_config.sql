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

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN vehicle_variant text ;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN free_trial_days integer NOT NULL default 7;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN enable_subscription boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.subscription_config DROP CONSTRAINT subscription_config_pkey;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD PRIMARY KEY ( service_name, vehicle_variant, merchant_operating_city_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ALTER COLUMN vehicle_variant SET DEFAULT 'AUTO_RICKSHAW';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ALTER COLUMN vehicle_variant SET NOT NULL;