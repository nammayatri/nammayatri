CREATE TABLE atlas_driver_offer_bpp.driver_fee ();

ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN amount_paid_by_coin double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN autopay_payment_stage text ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN bad_debt_declaration_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN bad_debt_recovery_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN bill_number integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN collected_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN collected_by text ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN end_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN fee_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN fee_without_discount double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN govt_charges integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN notification_retry_count integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN num_rides integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN offer_id text ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN overlay_sent boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN pay_by timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN plan_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN plan_mode text ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN plan_offer_title text ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN cgst double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN platform_fee double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN sgst double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN scheduler_try_count integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN service_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN special_zone_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN special_zone_ride_count integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN stage_updated_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN start_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN total_earnings integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN vehicle_number text ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_fee ALTER COLUMN merchant_operating_city_id DROP NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_fee ALTER COLUMN merchant_operating_city_id TYPE text;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_fee ALTER COLUMN merchant_operating_city_id TYPE character varying(36);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_fee ALTER COLUMN service_name DROP NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_fee ALTER COLUMN service_name SET NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_fee ALTER COLUMN service_name DROP NOT NULL;