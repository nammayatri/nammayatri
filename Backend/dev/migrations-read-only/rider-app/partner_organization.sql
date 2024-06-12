CREATE TABLE atlas_app.partner_organization ();

ALTER TABLE atlas_app.partner_organization ADD COLUMN api_key_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_app.partner_organization ADD COLUMN api_key_hash text NOT NULL;
ALTER TABLE atlas_app.partner_organization ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.partner_organization ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.partner_organization ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.partner_organization ADD COLUMN org_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.partner_organization ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.partner_organization ADD PRIMARY KEY ( api_key_hash);