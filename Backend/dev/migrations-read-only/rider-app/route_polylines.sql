CREATE TABLE atlas_app.route_polylines ();

ALTER TABLE atlas_app.route_polylines ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route_polylines ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_polylines ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_polylines ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_polylines ADD COLUMN polyline text ;
ALTER TABLE atlas_app.route_polylines ADD COLUMN route_id text NOT NULL;
ALTER TABLE atlas_app.route_polylines ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route_polylines ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.route_polylines ADD PRIMARY KEY ( id);
