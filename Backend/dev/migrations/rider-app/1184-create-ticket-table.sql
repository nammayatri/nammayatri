CREATE TABLE IF NOT EXISTS atlas_app.ticket (
    id character varying(36) NOT NULL PRIMARY KEY,
    status character varying(255),
    quote_id character varying(36) NOT NULL,
    search_request_id character varying(255) NOT NULL,
    bpp_order_id character varying(255),
    quantity integer DEFAULT 1 NOT NULL,
    price_per_adult numeric(30, 2) NOT NULL,
    total_price numeric(30, 2) NOT NULL,
    qr_data character varying(255), -- FIX LENGTH
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_app.fare_breakup ADD COLUMN entity_id character (36);
ALTER TABLE atlas_app.fare_breakup ADD COLUMN entity_type character varying (255) NOT NULL;