CREATE TABLE atlas_driver_offer_bpp.call_status ();

ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN call_error text ;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN call_id character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN call_service text ;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN conversation_duration bigint NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN dtmf_number_used character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN entity_id character varying (36)  default 'UNKNOWN';
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN merchant_id character varying (36) ;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN recording_url character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN status character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN call_attempt text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN merchant_operating_city_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN ai_call_analyzed boolean ;