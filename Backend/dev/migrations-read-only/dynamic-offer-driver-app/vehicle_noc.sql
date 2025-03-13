CREATE TABLE atlas_driver_offer_bpp.vehicle_noc ();

ALTER TABLE atlas_driver_offer_bpp.vehicle_noc ADD COLUMN document_image_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_noc ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_noc ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_noc ADD COLUMN noc_expiry timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_noc ADD COLUMN noc_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_noc ADD COLUMN noc_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_noc ADD COLUMN rc_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_noc ADD COLUMN verification_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_noc ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_noc ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_noc ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_noc ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_noc ADD PRIMARY KEY ( id);
