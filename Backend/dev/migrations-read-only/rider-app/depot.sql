CREATE TABLE atlas_app.depot ();

ALTER TABLE atlas_app.depot ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.depot ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.depot ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.depot ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.depot ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.depot ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.depot ADD PRIMARY KEY ( id);
