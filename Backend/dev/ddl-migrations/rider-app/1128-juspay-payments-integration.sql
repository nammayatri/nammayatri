CREATE TABLE atlas_app.payment_order (
    id character(36) PRIMARY KEY NOT NULL,
    short_id character varying (36) NOT NULL,
    customer_id character(36) NOT NULL REFERENCES atlas_app.person (id),
    merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id),
    amount integer NOT NULL,
    currency character varying(30) NOT NULL,
    status character varying(100) NOT NULL,
    web_payment_link text,
    iframe_payment_link text,
    mobile_payment_link text,
    client_auth_token_encrypted character varying(255) NOT NULL,
    client_auth_token_hash bytea NOT NULL,
    client_auth_token_expiry timestamp with time zone NOT NULL,
    get_upi_deep_links_option boolean,
    environment character varying(100),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_app.payment_transaction (
    id character(36) PRIMARY KEY NOT NULL,
    txn_uuid character varying (255) NOT NULL,
    payment_method_type character varying(100) NOT NULL,
    payment_method character varying(100) NOT NULL,
    resp_message character varying (255),
    resp_code character varying (255),
    gateway_reference_id character varying(100),
    order_id character(36) NOT NULL REFERENCES atlas_app.payment_order (id),
    merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id),
    amount numeric(30,2) NOT NULL,
    currency character varying(30) NOT NULL,
    date_created timestamp with time zone,
    status_id int NOT NULL,
    status character varying(100) NOT NULL,
    juspay_response text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
