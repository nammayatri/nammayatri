CREATE TABLE atlas_driver_offer_bpp.vehicle_puc ();

ALTER TABLE atlas_driver_offer_bpp.vehicle_puc ADD COLUMN document_image_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_puc ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_puc ADD COLUMN puc_expiry timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_puc ADD COLUMN rc_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_puc ADD COLUMN verification_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_puc ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_puc ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_puc ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_puc ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_puc ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_puc ADD COLUMN driver_id character varying(36) NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_puc ADD COLUMN test_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_puc ADD COLUMN puc_number_hash bytea ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_puc ADD COLUMN puc_number_encrypted character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_puc ALTER COLUMN puc_number_hash TYPE text;
ALTER TABLE atlas_driver_offer_bpp.vehicle_puc ALTER COLUMN puc_number_encrypted TYPE text;