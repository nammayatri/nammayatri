CREATE TABLE atlas_app.driver_offer ();

ALTER TABLE atlas_app.driver_offer ADD COLUMN bpp_quote_id character(36) NOT NULL;
ALTER TABLE atlas_app.driver_offer ADD COLUMN created_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.driver_offer ADD COLUMN distance_to_pickup double precision ;
ALTER TABLE atlas_app.driver_offer ADD COLUMN distance_to_pickup_value double precision ;
ALTER TABLE atlas_app.driver_offer ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_app.driver_offer ADD COLUMN driver_name character varying(255) NOT NULL;
ALTER TABLE atlas_app.driver_offer ADD COLUMN duration_to_pickup integer ;
ALTER TABLE atlas_app.driver_offer ADD COLUMN estimate_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.driver_offer ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.driver_offer ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.driver_offer ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.driver_offer ADD COLUMN rating double precision ;
ALTER TABLE atlas_app.driver_offer ADD COLUMN status character varying(255) NOT NULL default 'ACTIVE';
ALTER TABLE atlas_app.driver_offer ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.driver_offer ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.driver_offer ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.driver_offer ADD COLUMN fare_product_type character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_app.driver_offer ADD COLUMN gender text ;