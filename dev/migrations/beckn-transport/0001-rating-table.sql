CREATE TABLE atlas_transporter.rating (
    id character(36) NOT NULL,
    product_instance_id character varying(36) NOT NULL UNIQUE,
    rating_value bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE ONLY atlas_transporter."rating"
    ADD CONSTRAINT idx_rating_primary PRIMARY KEY (id);