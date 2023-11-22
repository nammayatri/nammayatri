CREATE TABLE atlas_app.public_transport_quote (
    id character(36) NOT NULL PRIMARY KEY,
    quote_id character(100) NOT NULL REFERENCES atlas_app.quote (id),
    total_estimated_distance double precision NOT NULL,
    total_duration double precision NOT NULL,
    vehicle_service_type text NOT NULL,
    stop_ids text NOT NULL,
    created_at timestamp with time zone NOT NULL
);

ALTER TABLE
    atlas_app.quote
ADD
    COLUMN public_transport_quote_id character(36) REFERENCES atlas_app.public_transport_quote (id);