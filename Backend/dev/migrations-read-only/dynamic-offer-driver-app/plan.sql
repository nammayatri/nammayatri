CREATE TABLE atlas_driver_offer_bpp.plan ();

ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN based_on_entity text NOT NULL default 'RIDE';
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN cgst_percentage double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN eligible_for_coin_discount boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN free_ride_count integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN frequency text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN is_deprecated boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN is_offer_applicable boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN max_amount integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN max_credit_limit integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN max_mandate_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN merchant_op_city_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN payment_mode text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN plan_base_amount text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN plan_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN registration_amount integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN service_name text NOT NULL default 'YATRI_SUBSCRIPTION';
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN sgst_percentage double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN subscribed_flag_toggle_allowed boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.plan ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.plan DROP COLUMN merchant_operating_city_id;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN variant text NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.plan ALTER COLUMN variant DROP NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN vehicle_variant text ;
ALTER TABLE atlas_driver_offer_bpp.plan DROP COLUMN variant;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN vehicle_category text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN listing_priority integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN allow_strike_off boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN product_ownership_amount double precision ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN payment_type text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN billing_type text ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN validity_in_days integer ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.plan ADD COLUMN is_fleet_owner_plan boolean ;

