CREATE TABLE atlas_driver_offer_bpp.search_retry (
    id character(36) NOT NULL,
    parent_search_id character(36)  NOT NULL REFERENCES atlas_driver_offer_bpp.search_request(id),
    retry_created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    retry_type TEXT NOT NULL,
    CONSTRAINT search_retry_pk PRIMARY KEY (id)
);
