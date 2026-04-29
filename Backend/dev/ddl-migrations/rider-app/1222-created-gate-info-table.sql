CREATE TABLE atlas_app.gate_info (
    id varchar(36) PRIMARY KEY,
    point text NOT NULL,
    special_location_id varchar(36) NOT NULL,
    default_driver_extra INT,
    name TEXT NOT NULL,
    geom public.geometry(MultiPolygon),
    address TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    can_queue_up_on_gate BOOLEAN NOT NULL
);

CREATE INDEX idx_point ON atlas_app.gate_info USING BTREE (point);

ALTER TABLE atlas_app.special_location ADD COLUMN merchant_operating_city_id varchar(36);
