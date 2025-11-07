CREATE TABLE atlas_app.translations ();

ALTER TABLE atlas_app.translations ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.translations ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.translations ADD COLUMN language text NOT NULL;
ALTER TABLE atlas_app.translations ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.translations ADD COLUMN message text NOT NULL;
ALTER TABLE atlas_app.translations ADD COLUMN message_key text NOT NULL;
ALTER TABLE atlas_app.translations ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.translations ADD PRIMARY KEY ( id);
