CREATE TABLE atlas_safety_dashboard.portal_configs ();

ALTER TABLE atlas_safety_dashboard.portal_configs ADD COLUMN config_name text NOT NULL;
ALTER TABLE atlas_safety_dashboard.portal_configs ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_safety_dashboard.portal_configs ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_safety_dashboard.portal_configs ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_safety_dashboard.portal_configs ADD COLUMN value text NOT NULL;
ALTER TABLE atlas_safety_dashboard.portal_configs ADD PRIMARY KEY ( id);