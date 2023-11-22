CREATE TABLE atlas_app.public_transport_quote_route_info (
    id character(36) NOT NULL PRIMARY KEY,
    route_id text NOT NULL,
    trip_id text NOT NULL,
    route_no text NOT NULL,
    route_name text NOT NULL,
    created_at timestamp with time zone NOT NULL
);

ALTER TABLE
    atlas_app.public_transport_quote
ADD
    COLUMN route_info_id character(36) REFERENCES atlas_app.public_transport_quote_route_info (id);