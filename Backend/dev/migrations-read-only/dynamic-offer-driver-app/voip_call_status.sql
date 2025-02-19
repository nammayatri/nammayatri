CREATE TABLE atlas_driver_offer_bpp.voip_call_status ();

ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN call_id character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN call_status character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN error_code bigint ;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN merchant_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN merchant_id character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN network_quality character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN network_type character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN ride_id character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD PRIMARY KEY ( id);



------- SQL updates -------


--- Now DSL don't allow dropping tables instead we will drop not null constraint if any .Please be careful while running ---
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ALTER COLUMN merchant_city_id DROP NOT NULL;
--- Drop section ends. Please check before running ---
