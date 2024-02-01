CREATE TABLE atlas_driver_offer_bpp.white_list_org (
    id character(36) NOT NULL,
    subscriber_id character(255) NOT NULL,
    type character varying(255),
    CONSTRAINT white_list_org_pkey PRIMARY KEY (id)
);

CREATE TABLE atlas_driver_offer_bpp.black_list_org (
    id character(36) NOT NULL,
    subscriber_id character(255) NOT NULL,
    type character varying(255),
    CONSTRAINT black_list_org_pkey PRIMARY KEY (id)
);

insert into atlas_driver_offer_bpp.white_list_org (id,subscriber_id,type) values ('45a16bb9-69e1-4dc6-a751-65b82a84dbc9', 'localhost/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'APP');

insert into atlas_driver_offer_bpp.white_list_org (id,subscriber_id,type) values ('45a16bb9-69e1-4dc6-a751-65b82a84dbc0', 'JUSPAY.BG.1', 'APP');

