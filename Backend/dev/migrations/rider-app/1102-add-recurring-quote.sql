CREATE TABLE atlas_app.recurring_quote (
    id character(36) NOT NULL,
    request_id character varying(255) NOT NULL,
    estimated_fare numeric(30,10) NOT NULL,
    discount double precision,
    estimated_total_fare numeric(30,2),
    provider_id character varying(255) NOT NULL,
    provider_url character varying(255) NOT NULL
    vehicle_variant character varying(60) DEFAULT ''::character varying NOT NULL,
    distance_to_nearest_driver double precision NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    ,CONSTRAINT  idx_16394_primary PRIMARY KEY (id)
);
ALTER TABLE atlas_app.quote OWNER TO atlas_app_user;

