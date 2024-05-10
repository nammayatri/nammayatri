CREATE TABLE atlas_driver_offer_bpp.search_try ();

ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN base_fare integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN customer_extra_fee integer ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN estimate_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN estimate_ids text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN is_scheduled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN message_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN request_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN search_repeat_counter integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN search_repeat_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN start_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN trip_category text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN vehicle_service_tier_name text ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN vehicle_service_tier_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN trip_category DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN is_scheduled DROP NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN vehicle_service_tier text NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN vehicle_service_tier_name DROP NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN estimate_id DROP NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN vehicle_service_tier_name SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN vehicle_variant text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try DROP COLUMN vehicle_service_tier;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN vehicle_service_tier_name DROP NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN estimate_ids DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN estimate_id SET NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN vehicle_variant TYPE character varying (255);
ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN search_repeat_type TYPE character varying (255);
ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN message_id TYPE character (36);
ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN estimate_ids TYPE ARRAY [];
ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN estimate_id TYPE character (36);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN customer_extra_fee SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN customer_extra_fee_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN base_fare_amount double precision ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN customer_extra_fee DROP NOT NULL;