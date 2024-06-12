CREATE TABLE atlas_app.partner_org_config ();

ALTER TABLE atlas_app.partner_org_config ADD COLUMN config_json json NOT NULL;
ALTER TABLE atlas_app.partner_org_config ADD COLUMN config_type character varying(255) NOT NULL;
ALTER TABLE atlas_app.partner_org_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.partner_org_config ADD COLUMN partner_org_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.partner_org_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.partner_org_config ADD PRIMARY KEY ( config_type, partner_org_id);