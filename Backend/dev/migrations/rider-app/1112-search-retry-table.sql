CREATE TABLE atlas_app.search_retry (
    id character(36) NOT NULL,
    parent_search_id  character(36) NOT NULL,
    retry_created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    retry_type TEXT NOT NULL,
    CONSTRAINT search_retry_pk PRIMARY KEY (id)
);
