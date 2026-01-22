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
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN fee_type text NOT NULL default 'RECURRING_INVOICE';
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN fee_without_discount double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN govt_charges integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN merchant_id character varying(36) NOT NULL default '96dd7f78-787e-4a0b-8675-e9e6fe93bb8f';
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN merchant_operating_city_id text ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN notification_retry_count integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN num_rides integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN offer_id character varying(100) ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN overlay_sent boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN pay_by timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN plan_id text ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN plan_mode text ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN plan_offer_title text ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN cgst numeric(30,2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN platform_fee numeric(30,2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN sgst numeric(30,2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN scheduler_try_count integer NOT NULL default 1;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN service_name text  default 'YATRI_SUBSCRIPTION';
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN special_zone_amount double precision NOT NULL default 0.0;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN special_zone_ride_count integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN stage_updated_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN start_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN status character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN total_earnings integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN vehicle_number text ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN total_earnings_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN govt_charges_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN currency character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN refunded_by text ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN refunded_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN refunded_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN refund_entity_id text ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN vehicle_category text ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN parent_driver_fee_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN has_child_driver_fees boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN split_of_driver_fee_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN sibling_fee_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN has_sibling boolean ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN valid_days integer ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN cancellation_penalty_amount double precision ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN added_to_fee_id character varying(36) ;





------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN collected_at_vendor_id text ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN driver_considered_in_payout_settlement_at timestamp with time zone ;