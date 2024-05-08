CREATE TABLE atlas_driver_offer_bpp.call_status ();

ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN call_error text ;
<<<<<<< HEAD
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN call_id character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN call_service text ;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN conversation_duration bigint NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN dtmf_number_used character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN entity_id character (36)  default 'UNKNOWN';
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN merchant_id character (36) ;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN recording_url character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN status character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD PRIMARY KEY ( id);
=======
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN call_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN call_service text ;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN conversation_duration integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN dtmf_number_used text ;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN entity_id text ;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN merchant_id text ;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN recording_url text ;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN call_status_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN status text NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.call_status ALTER COLUMN call_service TYPE integer;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.call_status ALTER COLUMN call_service TYPE text;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.call_status ALTER COLUMN status TYPE integer;
ALTER TABLE atlas_driver_offer_bpp.call_status DROP COLUMN call_status_id;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.call_status ALTER COLUMN status TYPE character varying (255);
ALTER TABLE atlas_driver_offer_bpp.call_status ALTER COLUMN recording_url TYPE character varying (255);
ALTER TABLE atlas_driver_offer_bpp.call_status ALTER COLUMN merchant_id TYPE character varying (36);
ALTER TABLE atlas_driver_offer_bpp.call_status ALTER COLUMN entity_id SET DEFAULT 'UNKNOWN';
ALTER TABLE atlas_driver_offer_bpp.call_status ALTER COLUMN entity_id TYPE character varying (36);
ALTER TABLE atlas_driver_offer_bpp.call_status ALTER COLUMN dtmf_number_used TYPE character varying (255);
ALTER TABLE atlas_driver_offer_bpp.call_status ALTER COLUMN conversation_duration TYPE bigint;
ALTER TABLE atlas_driver_offer_bpp.call_status ALTER COLUMN call_id TYPE character varying (255);
>>>>>>> 2ac75e3b6b (backend/enh/DSL/move-tables-dsl-driver-offer-person-part: CallStatus)
