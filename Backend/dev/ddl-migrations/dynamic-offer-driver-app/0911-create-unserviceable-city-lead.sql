CREATE TABLE atlas_driver_offer_bpp.unserviceable_city_lead (
    id character(36) NOT NULL PRIMARY KEY,
    merchant_id character(36) NOT NULL,
    name character varying(255) NOT NULL,
    mobile_number_encrypted character varying(255) NOT NULL,
    mobile_number_hash bytea NOT NULL,
    mobile_country_code character varying(8) NOT NULL,
    vehicle_category character varying(255) NOT NULL,
    lat double precision,
    lon double precision,
    device_token character varying(255),
    created_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_unserviceable_city_lead_mobile_hash_merchant ON atlas_driver_offer_bpp.unserviceable_city_lead USING btree (mobile_number_hash, merchant_id);
