CREATE TABLE atlas_driver_offer_bpp.communication_delivery ();

ALTER TABLE atlas_driver_offer_bpp.communication_delivery ADD COLUMN channel character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication_delivery ADD COLUMN communication_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication_delivery ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.communication_delivery ADD COLUMN delivered_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.communication_delivery ADD COLUMN failure_reason text ;
ALTER TABLE atlas_driver_offer_bpp.communication_delivery ADD COLUMN fleet_owner_id character varying (36) ;
ALTER TABLE atlas_driver_offer_bpp.communication_delivery ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication_delivery ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication_delivery ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication_delivery ADD COLUMN operator_id character varying (36) ;
ALTER TABLE atlas_driver_offer_bpp.communication_delivery ADD COLUMN read_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.communication_delivery ADD COLUMN recipient_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication_delivery ADD COLUMN recipient_role character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication_delivery ADD COLUMN status character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication_delivery ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.communication_delivery ADD PRIMARY KEY ( id);
