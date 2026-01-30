CREATE TABLE atlas_app.organization ();

ALTER TABLE atlas_app.organization ADD COLUMN contact_name character varying(255) NOT NULL;
ALTER TABLE atlas_app.organization ADD COLUMN contact_phone_number_encrypted character varying(255) ;
ALTER TABLE atlas_app.organization ADD COLUMN contact_phone_number_hash bytea ;
ALTER TABLE atlas_app.organization ADD COLUMN contact_role character varying(255) ;
ALTER TABLE atlas_app.organization ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.organization ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.organization ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.organization ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.organization ADD COLUMN organization_address text ;
ALTER TABLE atlas_app.organization ADD COLUMN organization_name character varying(255) NOT NULL;
ALTER TABLE atlas_app.organization ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.organization ADD PRIMARY KEY ( id);
CREATE INDEX organization_idx_organization_name ON atlas_app.organization USING btree (organization_name);