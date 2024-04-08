CREATE TABLE atlas_app.quote ();

ALTER TABLE atlas_app.quote ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.quote ADD COLUMN currency text ;
ALTER TABLE atlas_app.quote ADD COLUMN discount double precision ;
ALTER TABLE atlas_app.quote ADD COLUMN distance_to_nearest_driver integer ;
ALTER TABLE atlas_app.quote ADD COLUMN driver_offer_id text ;
ALTER TABLE atlas_app.quote ADD COLUMN fare_product_type text NOT NULL;
ALTER TABLE atlas_app.quote ADD COLUMN rental_details_id text ;
ALTER TABLE atlas_app.quote ADD COLUMN special_zone_quote_id text ;
ALTER TABLE atlas_app.quote ADD COLUMN estimated_fare double precision NOT NULL;
ALTER TABLE atlas_app.quote ADD COLUMN estimated_total_fare double precision NOT NULL;
ALTER TABLE atlas_app.quote ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.quote ADD COLUMN item_id text NOT NULL;
ALTER TABLE atlas_app.quote ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.quote ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.quote ADD COLUMN provider_id text NOT NULL;
ALTER TABLE atlas_app.quote ADD COLUMN provider_url text NOT NULL;

ALTER TABLE atlas_app.quote ADD COLUMN request_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.quote ADD COLUMN service_tier_name text ;
ALTER TABLE atlas_app.quote ADD COLUMN service_tier_short_desc text ;
ALTER TABLE atlas_app.quote ADD COLUMN special_location_tag text ;
ALTER TABLE atlas_app.quote ADD COLUMN trip_terms_id text ;
ALTER TABLE atlas_app.quote ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.quote ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.quote ADD COLUMN vehicle_service_tier_type text NOT NULL;
ALTER TABLE atlas_app.quote ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.quote ALTER COLUMN updated_at DROP NOT NULL;
ALTER TABLE atlas_app.quote ALTER COLUMN merchant_operating_city_id DROP NOT NULL;
ALTER TABLE atlas_app.quote ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_app.quote ADD COLUMN distance_to_nearest_driver_value double precision ;