CREATE TABLE atlas_driver_offer_bpp.vehicle_permit ();

ALTER TABLE atlas_driver_offer_bpp.vehicle_permit ADD COLUMN document_image_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_permit ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_permit ADD COLUMN issue_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_permit ADD COLUMN name_of_permit_holder text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_permit ADD COLUMN permit_expiry timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_permit ADD COLUMN permit_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_permit ADD COLUMN permit_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_permit ADD COLUMN purpose_of_journey text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_permit ADD COLUMN rc_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_permit ADD COLUMN region_covered text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_permit ADD COLUMN verification_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_permit ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_permit ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_permit ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_permit ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_permit ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_permit ADD COLUMN driver_id character varying(36) NOT NULL;