CREATE TABLE atlas_driver_offer_bpp.driver_go_home_request ();

ALTER TABLE atlas_driver_offer_bpp.driver_go_home_request ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_go_home_request ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_go_home_request ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_go_home_request ADD COLUMN lat double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_go_home_request ADD COLUMN lon double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_go_home_request ADD COLUMN reached_home boolean ;
ALTER TABLE atlas_driver_offer_bpp.driver_go_home_request ADD COLUMN num_cancellation integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_go_home_request ADD COLUMN status character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_go_home_request ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_go_home_request ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_go_home_request ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_go_home_request ADD COLUMN merchant_id character varying(36) ;