CREATE TABLE atlas_safety_dashboard.suspect_status_change_request ();

ALTER TABLE atlas_safety_dashboard.suspect_status_change_request ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_safety_dashboard.suspect_status_change_request ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect_status_change_request ADD COLUMN merchant_short_id text NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect_status_change_request ADD COLUMN reason_to_change text NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect_status_change_request ADD COLUMN req_status text NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect_status_change_request ADD COLUMN suspect_id text NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect_status_change_request ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_safety_dashboard.suspect_status_change_request ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_safety_dashboard.suspect_status_change_request ADD PRIMARY KEY ( id);