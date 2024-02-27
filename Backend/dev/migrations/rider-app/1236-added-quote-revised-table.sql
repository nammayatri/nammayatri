CREATE TABLE atlas_app.quote_revised (
    id character(36) NOT NULL,
    request_id character varying(255) NOT NULL,
    estimated_fare numeric(30,10) NOT NULL,
    provider_id character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    vehicle_variant character varying(60) DEFAULT ''::character varying NOT NULL,
    discount numeric(30,2),
    estimated_total_fare numeric(30,2),
    fare_product_type varchar(255) NOT NULL,
    distance_to_nearest_driver numeric(30,2),
    trip_terms_id character(36) REFERENCES atlas_app.trip_terms (id),
    driver_offer_id character(36) REFERENCES atlas_app.driver_offer (id),
    special_zone_quote_id character(36) REFERENCES atlas_app.special_zone_quote (id),
    -- total_fare numeric(30,2),
    provider_mobile_number character varying(255) NOT NULL,
    provider_name character varying(255) NOT NULL,
    provider_completed_rides_count integer NOT NULL,
    special_location_tag text,
    item_id text NOT NULL DEFAULT '',
    merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city (id),
    merchant_id character varying(36) NOT NULL REFERENCES atlas_app.merchant (id) DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51',
    provider_url character varying(255) NOT NULL,
    rental_details_id character varying(36)
);

ALTER TABLE atlas_app.quote_revised OWNER TO atlas_app_user;

ALTER TABLE atlas_app.quote_revised
  ADD CONSTRAINT quote_unique_reqid UNIQUE (request_id, provider_id);

UPDATE atlas_app.quote
    SET fare_product_type = 'DRIVER_OFFER'
    WHERE fare_product_type = 'AUTO';

UPDATE atlas_app.quote
    SET  vehicle_variant = 'AUTO_RICKSHAW'
    WHERE vehicle_variant = 'AUTO';
