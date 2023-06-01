CREATE TABLE atlas_app.callback_request (
    id character(36) PRIMARY KEY NOT NULL,
    merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id),
    customer_name character varying(255),
    customer_phone_encrypted character varying(255) NOT NULL,
    customer_phone_hash bytea NOT NULL,
    customer_mobile_country_code character varying(255) NOT NULL,
    status character varying(255) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
