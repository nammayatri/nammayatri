CREATE TABLE atlas_app.person_flow_status ();

ALTER TABLE atlas_app.person_flow_status ADD COLUMN flow_status JSON NOT NULL;
ALTER TABLE atlas_app.person_flow_status ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.person_flow_status ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person_flow_status ADD PRIMARY KEY ( person_id);