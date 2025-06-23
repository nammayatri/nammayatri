CREATE TABLE atlas_driver_offer_bpp.driver_plan ();

ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN auto_pay_status text ;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN coin_coverted_to_cash_left double precision NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN enable_service_usage_charge boolean  default true;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN last_payment_link_sent_at_ist_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN mandate_id text ;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN mandate_setup_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN merchant_id text ;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN merchant_op_city_id text ;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN payer_vpa text ;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN plan_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN plan_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN service_name text  default 'YATRI_SUBSCRIPTION';
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN rented_vehicle_number text ;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN total_coins_converted_cash double precision NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD PRIMARY KEY ( driver_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN vehicle_category text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN is_on_free_trial boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN is_category_level_subscription_enabled boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN total_amount_charged_for_service integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN last_bill_generated_at timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN waiver_off_percentage double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN waive_of_mode text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN waive_off_valid_till timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN waive_off_enabled_on timestamp with time zone ;