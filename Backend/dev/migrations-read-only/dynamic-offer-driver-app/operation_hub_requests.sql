CREATE TABLE atlas_driver_offer_bpp.operation_hub_requests ();

ALTER TABLE atlas_driver_offer_bpp.operation_hub_requests ADD COLUMN creator_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.operation_hub_requests ADD COLUMN fulfilled_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.operation_hub_requests ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.operation_hub_requests ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.operation_hub_requests ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.operation_hub_requests ADD COLUMN operation_hub_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.operation_hub_requests ADD COLUMN operator_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.operation_hub_requests ADD COLUMN registration_no text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.operation_hub_requests ADD COLUMN remarks text ;
ALTER TABLE atlas_driver_offer_bpp.operation_hub_requests ADD COLUMN request_status character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.operation_hub_requests ADD COLUMN request_type character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.operation_hub_requests ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.operation_hub_requests ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.operation_hub_requests ADD PRIMARY KEY ( id);
