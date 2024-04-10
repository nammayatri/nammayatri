CREATE TABLE atlas_safety_dashboard.suspect ();

ALTER TABLE atlas_safety_dashboard.suspect ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_safety_dashboard.suspect ADD COLUMN dl text ;
ALTER TABLE atlas_safety_dashboard.suspect ADD COLUMN first_name text NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect ADD COLUMN flag_updated_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect ADD COLUMN flagged_by json NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect ADD COLUMN flagged_counter integer NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect ADD COLUMN flagged_status text NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect ADD COLUMN last_name text NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect ADD COLUMN status_changed_reason text ;
ALTER TABLE atlas_safety_dashboard.suspect ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_safety_dashboard.suspect ADD COLUMN voter_id text ;
ALTER TABLE atlas_safety_dashboard.suspect ADD PRIMARY KEY ( id);