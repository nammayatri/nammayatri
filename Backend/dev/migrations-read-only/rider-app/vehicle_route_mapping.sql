CREATE TABLE atlas_app.vehicle_route_mapping ();

ALTER TABLE atlas_app.vehicle_route_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.vehicle_route_mapping ADD COLUMN route_id text NOT NULL;
ALTER TABLE atlas_app.vehicle_route_mapping ADD COLUMN service text NOT NULL;
ALTER TABLE atlas_app.vehicle_route_mapping ADD COLUMN shift text NOT NULL;
ALTER TABLE atlas_app.vehicle_route_mapping ADD COLUMN type_of_service text NOT NULL;
ALTER TABLE atlas_app.vehicle_route_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.vehicle_route_mapping ADD COLUMN vehicle_no text NOT NULL;
ALTER TABLE atlas_app.vehicle_route_mapping ADD PRIMARY KEY ( vehicle_no);
