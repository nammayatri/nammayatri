CREATE TABLE atlas_driver_offer_bpp.rating ();

ALTER TABLE atlas_driver_offer_bpp.rating ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.rating ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rating ADD COLUMN feedback_details text ;
ALTER TABLE atlas_driver_offer_bpp.rating ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rating ADD COLUMN is_safe boolean ;
ALTER TABLE atlas_driver_offer_bpp.rating ADD COLUMN issue_id text ;
ALTER TABLE atlas_driver_offer_bpp.rating ADD COLUMN rating_value integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rating ADD COLUMN ride_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rating ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.rating ADD COLUMN was_offered_assistance boolean ;
ALTER TABLE atlas_driver_offer_bpp.rating ADD PRIMARY KEY ( id, ride_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.rating DROP CONSTRAINT rating_pkey;
ALTER TABLE atlas_driver_offer_bpp.rating ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.rating ADD COLUMN is_favourite boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.rating ADD COLUMN media_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.rating ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.rating ADD COLUMN merchant_id character varying(36) ;