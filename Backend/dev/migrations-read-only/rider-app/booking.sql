CREATE TABLE atlas_app.booking ();

ALTER TABLE atlas_app.booking ADD COLUMN distance integer ;
ALTER TABLE atlas_app.booking ADD COLUMN fare_product_type text NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN otp_code text ;
ALTER TABLE atlas_app.booking ADD COLUMN stop_location_id text ;
ALTER TABLE atlas_app.booking ADD COLUMN to_location_id text ;
ALTER TABLE atlas_app.booking ADD COLUMN bpp_booking_id character varying(36) ;
ALTER TABLE atlas_app.booking ADD COLUMN client_id character varying(36) ;
ALTER TABLE atlas_app.booking ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.booking ADD COLUMN discount double precision ;
ALTER TABLE atlas_app.booking ADD COLUMN estimated_distance integer ;
ALTER TABLE atlas_app.booking ADD COLUMN estimated_duration integer ;
ALTER TABLE atlas_app.booking ADD COLUMN currency text ;
ALTER TABLE atlas_app.booking ADD COLUMN estimated_fare double precision NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN estimated_total_fare double precision NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN from_location_id text ;
ALTER TABLE atlas_app.booking ADD COLUMN fulfillment_id text ;
ALTER TABLE atlas_app.booking ADD COLUMN id character varying(36) NOT NULL;

ALTER TABLE atlas_app.booking ADD COLUMN is_scheduled boolean NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN item_id text NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN payment_method_id character varying(36) ;
ALTER TABLE atlas_app.booking ADD COLUMN payment_status text ;
ALTER TABLE atlas_app.booking ADD COLUMN payment_url text ;
ALTER TABLE atlas_app.booking ADD COLUMN primary_exophone text NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN provider_id text NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN provider_url text NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN quote_id character varying(36) ;
ALTER TABLE atlas_app.booking ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN service_tier_name text ;
ALTER TABLE atlas_app.booking ADD COLUMN special_location_tag text ;
ALTER TABLE atlas_app.booking ADD COLUMN start_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN transaction_id text NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN trip_terms_id text ;
ALTER TABLE atlas_app.booking ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.booking ADD COLUMN vehicle_variant text NOT NULL;
ALTER TABLE atlas_app.booking ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.booking ALTER COLUMN merchant_operating_city_id DROP NOT NULL;
ALTER TABLE atlas_app.booking ALTER COLUMN is_scheduled DROP NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.booking ALTER COLUMN discount TYPE numeric(30,2);
ALTER TABLE atlas_app.booking ADD COLUMN service_tier_short_desc text ;
ALTER TABLE atlas_app.booking ADD COLUMN rider_transaction_id text NOT NULL;
ALTER TABLE atlas_app.booking ADD COLUMN estimated_distance_value double precision ;
ALTER TABLE atlas_app.booking ADD COLUMN distance_value double precision ;
ALTER TABLE atlas_app.booking ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_app.booking DROP COLUMN transaction_id;