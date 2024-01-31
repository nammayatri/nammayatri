CREATE TABLE atlas_app.white_list_org (
    id character(36) NOT NULL,
    subscriber_id character(36) NOT NULL,
    type character varying(255),
    CONSTRAINT white_list_org_pkey PRIMARY KEY (id)
);
