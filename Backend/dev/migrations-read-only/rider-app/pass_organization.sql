CREATE TABLE atlas_app.pass_organization ();

ALTER TABLE atlas_app.pass_organization ADD COLUMN address text ;
ALTER TABLE atlas_app.pass_organization ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pass_organization ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_organization ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_organization ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_organization ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.pass_organization ADD COLUMN pass_enum text NOT NULL;
ALTER TABLE atlas_app.pass_organization ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_organization ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pass_organization ADD PRIMARY KEY ( id);
CREATE INDEX pass_organization_idx_person_id ON atlas_app.pass_organization USING btree (person_id);