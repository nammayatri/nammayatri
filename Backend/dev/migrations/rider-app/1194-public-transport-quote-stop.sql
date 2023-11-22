CREATE TABLE atlas_app.public_transport_quote_stop (
    id character(36) NOT NULL PRIMARY KEY,
    stop_name text NULL,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    scheduled_time text NOT NULL,
    created_at timestamp with time zone NOT NULL
);

ALTER TABLE
    atlas_app.public_transport_quote
ADD
    COLUMN start_stop_id character(36) REFERENCES atlas_app.public_transport_quote_stop (id);

ALTER TABLE
    atlas_app.public_transport_quote
ADD
    COLUMN end_stop_id character(36) REFERENCES atlas_app.public_transport_quote_stop (id);
