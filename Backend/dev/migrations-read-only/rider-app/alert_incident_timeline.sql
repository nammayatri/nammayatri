CREATE TABLE atlas_app.alert_incident_timeline ();

ALTER TABLE atlas_app.alert_incident_timeline ADD COLUMN attachments jsonb ;
ALTER TABLE atlas_app.alert_incident_timeline ADD COLUMN content text ;
ALTER TABLE atlas_app.alert_incident_timeline ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.alert_incident_timeline ADD COLUMN event_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.alert_incident_timeline ADD COLUMN event_type character varying(50) NOT NULL;
ALTER TABLE atlas_app.alert_incident_timeline ADD COLUMN id character(36) NOT NULL;
ALTER TABLE atlas_app.alert_incident_timeline ADD COLUMN incident_id character(36) NOT NULL;
ALTER TABLE atlas_app.alert_incident_timeline ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.alert_incident_timeline ADD PRIMARY KEY ( id);
