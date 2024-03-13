CREATE TABLE atlas_app.call_status ();

ALTER TABLE atlas_app.call_status ADD COLUMN call_error text ;
ALTER TABLE atlas_app.call_status ADD COLUMN call_id text NOT NULL;
ALTER TABLE atlas_app.call_status ADD COLUMN call_service text ;
ALTER TABLE atlas_app.call_status ADD COLUMN conversation_duration integer NOT NULL;
ALTER TABLE atlas_app.call_status ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.call_status ADD COLUMN dtmf_number_used text ;
ALTER TABLE atlas_app.call_status ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.call_status ADD COLUMN merchant_id text ;
ALTER TABLE atlas_app.call_status ADD COLUMN recording_url text ;
ALTER TABLE atlas_app.call_status ADD COLUMN ride_id character varying(36) ;
ALTER TABLE atlas_app.call_status ADD COLUMN status integer NOT NULL;
ALTER TABLE atlas_app.call_status ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.call_status ADD PRIMARY KEY ( id);