CREATE TABLE atlas_app.alert_incident ();

ALTER TABLE atlas_app.alert_incident ADD COLUMN alert_name text NOT NULL;
ALTER TABLE atlas_app.alert_incident ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.alert_incident ADD COLUMN description text ;
ALTER TABLE atlas_app.alert_incident ADD COLUMN downtime_seconds integer ;
ALTER TABLE atlas_app.alert_incident ADD COLUMN external_url character varying(500) ;
ALTER TABLE atlas_app.alert_incident ADD COLUMN firing_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.alert_incident ADD COLUMN id character(36) NOT NULL;
ALTER TABLE atlas_app.alert_incident ADD COLUMN raw_payload text ;
ALTER TABLE atlas_app.alert_incident ADD COLUMN receiver character varying(255) ;
ALTER TABLE atlas_app.alert_incident ADD COLUMN resolved_time timestamp with time zone ;
ALTER TABLE atlas_app.alert_incident ADD COLUMN service_name character varying(255) NOT NULL;
ALTER TABLE atlas_app.alert_incident ADD COLUMN severity character varying(50) ;
ALTER TABLE atlas_app.alert_incident ADD COLUMN status character varying(50) NOT NULL;
ALTER TABLE atlas_app.alert_incident ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.alert_incident ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.alert_incident ALTER COLUMN alert_name TYPE character varying(255);
ALTER TABLE atlas_app.alert_incident ADD COLUMN alert_group character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_app.alert_incident ADD COLUMN is_manually_entered boolean ;