CREATE TABLE atlas_driver_offer_bpp.subscription_purchase ();

ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN enable_service_usage_charge boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN expiry_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN owner_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN owner_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN payment_order_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN plan_fee double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN plan_frequency text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN plan_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN plan_ride_credit double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN purchase_timestamp timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN service_name text NOT NULL default 'PREPAID_SUBSCRIPTION';
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN vehicle_category text ;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN waive_of_mode text NOT NULL default 'NO_WAIVE_OFF';
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN waive_off_enabled_on timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN waive_off_valid_till timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN waiver_off_percentage double precision NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_purchase ADD COLUMN finance_invoice_id character varying(36) ;