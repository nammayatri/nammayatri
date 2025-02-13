CREATE TABLE atlas_app.stage ();

ALTER TABLE atlas_app.stage ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.stage ADD COLUMN input_data_type text NOT NULL;
ALTER TABLE atlas_app.stage ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.stage ADD COLUMN "order" integer NOT NULL;
ALTER TABLE atlas_app.stage ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.stage ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.stage ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.stage ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.stage ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.stage ADD PRIMARY KEY ( id);
