CREATE TABLE atlas_safety_dashboard.suspect_status_history ();

ALTER TABLE atlas_safety_dashboard.suspect_status_history ADD COLUMN admin_approval text ;
ALTER TABLE atlas_safety_dashboard.suspect_status_history ADD COLUMN approved_by text ;
ALTER TABLE atlas_safety_dashboard.suspect_status_history ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_safety_dashboard.suspect_status_history ADD COLUMN dl text ;
ALTER TABLE atlas_safety_dashboard.suspect_status_history ADD COLUMN first_name text ;
ALTER TABLE atlas_safety_dashboard.suspect_status_history ADD COLUMN flagged_by text[] ;
ALTER TABLE atlas_safety_dashboard.suspect_status_history ADD COLUMN flagged_status text NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect_status_history ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect_status_history ADD COLUMN last_name text ;
ALTER TABLE atlas_safety_dashboard.suspect_status_history ADD COLUMN merchant_short_id text ;
ALTER TABLE atlas_safety_dashboard.suspect_status_history ADD COLUMN status_changed_reason text ;
ALTER TABLE atlas_safety_dashboard.suspect_status_history ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_safety_dashboard.suspect_status_history ADD COLUMN voter_id text ;
ALTER TABLE atlas_safety_dashboard.suspect_status_history ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_safety_dashboard.suspect_status_history ADD PRIMARY KEY ( id);