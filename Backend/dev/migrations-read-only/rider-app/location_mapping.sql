CREATE TABLE atlas_app.location_mapping ();

ALTER TABLE atlas_app.location_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.location_mapping ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_app.location_mapping ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.location_mapping ADD COLUMN location_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.location_mapping ADD COLUMN merchant_id character varying(36);
ALTER TABLE atlas_app.location_mapping ADD COLUMN merchant_operating_city_id character varying(36);
ALTER TABLE atlas_app.location_mapping ADD COLUMN "order" integer NOT NULL;
ALTER TABLE atlas_app.location_mapping ADD COLUMN tag text NOT NULL;
ALTER TABLE atlas_app.location_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.location_mapping ADD COLUMN version text NOT NULL;
ALTER TABLE atlas_app.location_mapping ADD PRIMARY KEY ( id);