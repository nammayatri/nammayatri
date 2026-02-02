CREATE TABLE atlas_app.sos ();

ALTER TABLE atlas_app.sos ADD COLUMN entity_type text ;
ALTER TABLE atlas_app.sos ADD COLUMN flow text NOT NULL;
ALTER TABLE atlas_app.sos ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.sos ADD COLUMN media_files text[] ;
ALTER TABLE atlas_app.sos ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.sos ADD COLUMN ride_id character varying(36) ;
ALTER TABLE atlas_app.sos ADD COLUMN sos_state text ;
ALTER TABLE atlas_app.sos ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.sos ADD COLUMN ticket_id text ;
ALTER TABLE atlas_app.sos ADD COLUMN tracking_expires_at timestamp with time zone ;
ALTER TABLE atlas_app.sos ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.sos ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.sos ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.sos ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.sos ADD PRIMARY KEY ( id);
