CREATE TABLE atlas_app.route_stop_mapping ();

ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN route_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN sequence integer NOT NULL;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN stop_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN time_bounds text NOT NULL default 'Unbounded';
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route_stop_mapping ADD PRIMARY KEY ( id);