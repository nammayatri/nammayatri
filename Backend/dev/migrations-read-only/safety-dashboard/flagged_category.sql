CREATE TABLE atlas_safety_dashboard.flagged_category ();

ALTER TABLE atlas_safety_dashboard.flagged_category ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_safety_dashboard.flagged_category ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_safety_dashboard.flagged_category ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_safety_dashboard.flagged_category ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_safety_dashboard.flagged_category ADD PRIMARY KEY ( id);