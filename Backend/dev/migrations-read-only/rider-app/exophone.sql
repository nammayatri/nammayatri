CREATE TABLE atlas_app.exophone ();

ALTER TABLE atlas_app.exophone ADD COLUMN backup_phone text NOT NULL;
ALTER TABLE atlas_app.exophone ADD COLUMN call_service text NOT NULL default 'Exotel';
ALTER TABLE atlas_app.exophone ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.exophone ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.exophone ADD COLUMN is_primary_down boolean NOT NULL;
ALTER TABLE atlas_app.exophone ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.exophone ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.exophone ADD COLUMN primary_phone text NOT NULL;
ALTER TABLE atlas_app.exophone ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.exophone ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.exophone ADD COLUMN enable_alternate_number boolean ;