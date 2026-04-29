CREATE TABLE atlas_app.estimate_breakup (
    id character(36) NOT NULL PRIMARY KEY,
    estimate_id character (36) NOT NULL REFERENCES atlas_app.estimate (id),
    title character varying(255) NOT NULL,
    price_currency character varying(255) NOT NULL,
    price_value numeric(30,2) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
