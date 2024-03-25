CREATE TABLE atlas_app.white_list_org (
    id character(36) NOT NULL,
    subscriber_id character(255) NOT NULL,
    type character varying(255),
    CONSTRAINT white_list_org_pkey PRIMARY KEY (id)
);

insert into atlas_app.white_list_org (id,subscriber_id,type) values ('45a16bb9-69e1-4dc6-a751-65b82a84dbc9', 'api.sandbox.moving.tech/dev/dobpp/beckn/7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'APP');

insert into atlas_app.white_list_org (id,subscriber_id,type) values ('45a16bb9-69e1-4dc6-a751-65b82a84dbc0', 'JUSPAY.BG.1', 'APP');

