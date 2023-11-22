CREATE TABLE IF NOT EXISTS atlas_app.ticket (
    id character varying(36) NOT NULL PRIMARY KEY,
    status character varying(255),
    quote_id character varying(36) NOT NULL REFERENCES atlas_app.quote(id),
    search_request_id character varying(255) NOT NULL REFERENCES atlas_app.search_request(id),
    bpp_order_id character varying(255),
    item_id character varying(36) NOT NULL,
    bpp_ticket_id character varying(255),
    fulfillment_id character varying(255),
    payment_url character varying(255),
    provider_id character varying(36) NOT NULL,
    provider_url character varying(255) NOT NULL,
    quantity integer DEFAULT 1 NOT NULL,
    from_location_id character varying(36) NOT NULL REFERENCES atlas_app.location(id),
    price_per_adult numeric(30, 2) NOT NULL,
    total_price numeric(30, 2) NOT NULL,
    qr_data character varying(255), -- FIX LENGTH
    merchant_operating_city_id character varying(36) NOT NULL REFERENCES atlas_app.merchant_operating_city(id),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_app.fare_breakup ADD COLUMN entity_id character (36);
ALTER TABLE atlas_app.fare_breakup ADD COLUMN entity_type character varying (255) NOT NULL;