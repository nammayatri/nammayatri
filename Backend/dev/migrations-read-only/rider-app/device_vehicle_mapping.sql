CREATE TABLE atlas_app.device_vehicle_mapping ();

ALTER TABLE atlas_app.device_vehicle_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.device_vehicle_mapping ADD COLUMN device_id text NOT NULL;
ALTER TABLE atlas_app.device_vehicle_mapping ADD COLUMN gtfs_id text NOT NULL;
ALTER TABLE atlas_app.device_vehicle_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.device_vehicle_mapping ADD COLUMN vehicle_no text NOT NULL;
ALTER TABLE atlas_app.device_vehicle_mapping ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.device_vehicle_mapping ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.device_vehicle_mapping ADD PRIMARY KEY ( device_id);
CREATE INDEX device_vehicle_mapping_idx_vehicle_no ON atlas_app.device_vehicle_mapping USING btree (vehicle_no);
CREATE INDEX device_vehicle_mapping_idx_gtfs_id ON atlas_app.device_vehicle_mapping USING btree (gtfs_id);