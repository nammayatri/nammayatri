CREATE TABLE atlas_driver_offer_bpp.special_zone_queue_request (
    id character(36) NOT NULL PRIMARY KEY,
    driver_id character(36) NOT NULL,
    gate_id text NOT NULL,
    special_location_id text NOT NULL,
    merchant_id character(36) NOT NULL,
    merchant_operating_city_id character(36) NOT NULL,
    status character varying(50) NOT NULL DEFAULT 'Active',
    response character varying(50),
    valid_till timestamp with time zone NOT NULL,
    gate_name text NOT NULL,
    special_location_name text NOT NULL,
    vehicle_type text NOT NULL,
    created_at timestamp with time zone NOT NULL DEFAULT now(),
    updated_at timestamp with time zone NOT NULL DEFAULT now()
);

CREATE INDEX idx_szqr_driver_status ON atlas_driver_offer_bpp.special_zone_queue_request (driver_id, status);
