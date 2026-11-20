CREATE TABLE atlas_app.towing_vehicle ();

ALTER TABLE atlas_app.towing_vehicle ADD COLUMN agency_entity_id text NOT NULL;
ALTER TABLE atlas_app.towing_vehicle ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.towing_vehicle ADD COLUMN is_active boolean NOT NULL default true;
ALTER TABLE atlas_app.towing_vehicle ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.towing_vehicle ADD COLUMN vehicle_number text NOT NULL;
ALTER TABLE atlas_app.towing_vehicle ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.towing_vehicle ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.towing_vehicle ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.towing_vehicle ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.towing_vehicle ADD PRIMARY KEY ( id);
