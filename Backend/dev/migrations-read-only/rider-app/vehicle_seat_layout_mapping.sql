CREATE TABLE atlas_app.vehicle_seat_layout_mapping ();

ALTER TABLE atlas_app.vehicle_seat_layout_mapping ADD COLUMN gtfs_id text NOT NULL;
ALTER TABLE atlas_app.vehicle_seat_layout_mapping ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.vehicle_seat_layout_mapping ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.vehicle_seat_layout_mapping ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.vehicle_seat_layout_mapping ADD COLUMN seat_layout_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.vehicle_seat_layout_mapping ADD COLUMN vehicle_no text NOT NULL;
ALTER TABLE atlas_app.vehicle_seat_layout_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.vehicle_seat_layout_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.vehicle_seat_layout_mapping ADD PRIMARY KEY ( id);
ALTER TABLE atlas_app.vehicle_seat_layout_mapping ADD CONSTRAINT vehicle_seat_layout_mapping_unique_idx_gtfs_id_vehicle_no UNIQUE (gtfs_id, vehicle_no);