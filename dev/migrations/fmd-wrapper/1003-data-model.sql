CREATE TABLE atlas_fmd_wrapper.search_request_bak_000 AS TABLE atlas_fmd_wrapper.search_request;
CREATE TABLE atlas_fmd_wrapper.organization_bak_000 AS TABLE atlas_fmd_wrapper.organization;
DROP TABLE atlas_fmd_wrapper.search_request;
DROP TABLE atlas_fmd_wrapper.organization;

CREATE TABLE atlas_fmd_wrapper.dunzo_creds (
    id character(36) NOT NULL PRIMARY KEY,
    client_id character(36) NOT NULL UNIQUE,
    client_secret character(36) NOT NULL
);

-- do we need gateway with dunzo_creds_id=null in organization table?
CREATE TABLE atlas_fmd_wrapper.organization (
    id character(36) NOT NULL PRIMARY KEY,
    short_id character(36) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    dunzo_creds_id character(36) NOT NULL REFERENCES atlas_fmd_wrapper.dunzo_creds (id)
);

CREATE TABLE atlas_fmd_wrapper.address (
    id character(36) NOT NULL PRIMARY KEY,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    country character varying(255) NOT NULL,
    state character varying(255) NOT NULL,
    city character varying(255) NOT NULL,
    street character varying(255) NOT NULL,
    building character varying(255),
    door character varying(255) NOT NULL,
    name character varying(255),
    pincode character varying(255) NOT NULL,
    instructions character varying(255),
    search_request_id_tmp character(36) NOT NULL, -- this only used in migration script
    is_start_tmp boolean NOT NULL -- this only used in migration script
);

CREATE TABLE atlas_fmd_wrapper.person (
    id character(36) NOT NULL PRIMARY KEY,
    name character varying(255) NOT NULL,
    phone character varying(255) NOT NULL,
    search_request_id_tmp character(36) NOT NULL, -- this only used in migration script
    is_start_tmp boolean NOT NULL -- this only used in migration script
);

CREATE TABLE atlas_fmd_wrapper.delivery (
    id character(36) NOT NULL PRIMARY KEY,
    order_id character(36) NOT NULL,
    delivery_service_order_id character(36) NOT NULL,
    bap_id character(36) NOT NULL REFERENCES atlas_fmd_wrapper.organization (id),
    status character varying(255) NOT NULL,
    sender_id character(36) NOT NULL REFERENCES atlas_fmd_wrapper.person (id),
    receiver_id character(36) NOT NULL REFERENCES atlas_fmd_wrapper.person (id),
    pickup_address_id character(36) NOT NULL REFERENCES atlas_fmd_wrapper.address (id),
    drop_address_id character(36) NOT NULL REFERENCES atlas_fmd_wrapper.address (id),
    category_id integer NOT NULL,
    delivery_price double precision,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

INSERT INTO atlas_fmd_wrapper.dunzo_creds (id, client_Id, client_secret)
    (SELECT
        md5(random()::text || clock_timestamp()::text)::uuid,
        (T1.info :: json) ->> 'dzClientId',
        (T1.info :: json) ->> 'dzClientSecret'
    FROM atlas_fmd_wrapper.organization_bak_000 AS T1
    WHERE (T1.info :: json) ->> 'dzClientId' IS NOT NULL
        AND (T1.info :: json) ->> 'dzClientSecret' IS NOT NULL)
    ON CONFLICT DO NOTHING;

--creds for mock
INSERT INTO atlas_fmd_wrapper.dunzo_creds (id, client_Id, client_secret) VALUES
    ('00000000-0000-0000-0000-000000000000',
    '00000000-0000-0000-0000-000000000000',
    '00000000-0000-0000-0000-000000000000');

INSERT INTO atlas_fmd_wrapper.organization (id, short_id, created_at, updated_at, dunzo_creds_id)
    SELECT
        T1.id,
        T1.short_id,
        T1.created_at,
        T1.updated_at,
		T2.id
    FROM atlas_fmd_wrapper.organization_bak_000 AS T1 INNER JOIN atlas_fmd_wrapper.dunzo_creds AS T2
        ON T2.client_id = (T1.info :: json) ->> 'dzClientId';

INSERT INTO atlas_fmd_wrapper.address (id, lat, lon, country, state, city, street, building, door, name, pincode, instructions, search_request_id_tmp, is_start_tmp)
    SELECT
        md5(random()::text || clock_timestamp()::text)::uuid,
        ((regexp_split_to_array ((T1.udf1 :: json) -> 'fulfillment' -> 'start' -> 'location' ->> 'gps',  ',[[:space:]]' :: text)) [1]) :: double precision,
        ((regexp_split_to_array ((T1.udf1 :: json) -> 'fulfillment' -> 'start' -> 'location' ->> 'gps',  ',[[:space:]]' :: text)) [2]) :: double precision,
        COALESCE ((T1.udf1 :: json) -> 'fulfillment' -> 'start' -> 'location'  -> 'address' ->> 'country', 'UNKNOWN'),
        COALESCE ((T1.udf1 :: json) -> 'fulfillment' -> 'start' -> 'location'  -> 'address' ->> 'state', 'UNKNOWN'),
        COALESCE ((T1.udf1 :: json) -> 'fulfillment' -> 'start' -> 'location'  -> 'address' ->> 'city', 'UNKNOWN'),
        COALESCE ((T1.udf1 :: json) -> 'fulfillment' -> 'start' -> 'location'  -> 'address' ->> 'street', 'UNKNOWN'),
        (T1.udf1 :: json) -> 'fulfillment' -> 'start' -> 'location'  -> 'address' ->> 'building',
        COALESCE ((T1.udf1 :: json) -> 'fulfillment' -> 'start' -> 'location'  -> 'address' ->> 'door', 'UNKNOWN'),
        (T1.udf1 :: json) -> 'fulfillment' -> 'start' -> 'location'  -> 'address' ->> 'name',
        COALESCE ((T1.udf1 :: json) -> 'fulfillment' -> 'start' -> 'location'  -> 'address' ->> 'area_code', 'UNKNOWN'),
        (T1.udf1 :: json) -> 'fulfillment' -> 'start' -> 'instructions'  ->> 'name',
        T1.id,
        true
    FROM atlas_fmd_wrapper.search_request_bak_000 AS T1;

INSERT INTO atlas_fmd_wrapper.address (id, lat, lon, country, state, city, street, building, door, name, pincode, instructions, search_request_id_tmp, is_start_tmp)
    SELECT
        md5(random()::text || clock_timestamp()::text)::uuid,
        ((regexp_split_to_array ((T1.udf1 :: json) -> 'fulfillment' -> 'end' -> 'location' ->> 'gps',  ',[[:space:]]' :: text)) [1]) :: double precision,
        ((regexp_split_to_array ((T1.udf1 :: json) -> 'fulfillment' -> 'end' -> 'location' ->> 'gps',  ',[[:space:]]' :: text)) [2]) :: double precision,
        COALESCE ((T1.udf1 :: json) -> 'fulfillment' -> 'end' -> 'location'  -> 'address' ->> 'country', 'UNKNOWN'),
        COALESCE ((T1.udf1 :: json) -> 'fulfillment' -> 'end' -> 'location'  -> 'address' ->> 'state', 'UNKNOWN'),
        COALESCE ((T1.udf1 :: json) -> 'fulfillment' -> 'end' -> 'location'  -> 'address' ->> 'city', 'UNKNOWN'),
        COALESCE ((T1.udf1 :: json) -> 'fulfillment' -> 'end' -> 'location'  -> 'address' ->> 'street', 'UNKNOWN'),
        (T1.udf1 :: json) -> 'fulfillment' -> 'end' -> 'location'  -> 'address' ->> 'building',
        COALESCE ((T1.udf1 :: json) -> 'fulfillment' -> 'end' -> 'location'  -> 'address' ->> 'door', 'UNKNOWN'),
        (T1.udf1 :: json) -> 'fulfillment' -> 'end' -> 'location'  -> 'address' ->> 'name',
        COALESCE ((T1.udf1 :: json) -> 'fulfillment' -> 'end' -> 'location'  -> 'address' ->> 'area_code', 'UNKNOWN'),
        (T1.udf1 :: json) -> 'fulfillment' -> 'end' -> 'instructions'  ->> 'name',
        T1.id,
        false
    FROM atlas_fmd_wrapper.search_request_bak_000 AS T1;

INSERT INTO atlas_fmd_wrapper.person (id, name, phone, search_request_id_tmp, is_start_tmp)
    SELECT
        md5(random()::text || clock_timestamp()::text)::uuid,
        COALESCE (
            (T1.udf1 :: json) -> 'fulfillment' -> 'start' -> 'person' -> 'name' ->> 'getName',
            (T1.udf1 :: json) -> 'fulfillment' -> 'start' -> 'person' ->> 'name',
            'UNKNOWN'
        ),
        COALESCE ((T1.udf1 :: json) -> 'fulfillment' -> 'start' -> 'contact' ->> 'phone', 'UNKNOWN'),
        T1.id,
        true
    FROM atlas_fmd_wrapper.search_request_bak_000 AS T1;

INSERT INTO atlas_fmd_wrapper.person (id, name, phone, search_request_id_tmp, is_start_tmp)
    SELECT
        md5(random()::text || clock_timestamp()::text)::uuid,
        COALESCE (
            (T1.udf1 :: json) -> 'fulfillment' -> 'end' -> 'person' -> 'name' ->> 'getName',
            (T1.udf1 :: json) -> 'fulfillment' -> 'end' -> 'person' ->> 'name',
            'UNKNOWN'
        ),
        COALESCE ((T1.udf1 :: json) -> 'fulfillment' -> 'end' -> 'contact' ->> 'phone', 'UNKNOWN'),
        T1.id,
        false
    FROM atlas_fmd_wrapper.search_request_bak_000 AS T1;

INSERT INTO atlas_fmd_wrapper.delivery (
        id,
        order_id,
        delivery_service_order_id,
        bap_id,
        status,
        sender_id,
        receiver_id,
        pickup_address_id,
        drop_address_id,
        category_id,
        delivery_price,
        created_at,
        updated_at
    )
    SELECT
        T1.id,
        COALESCE ((T1.udf1 :: json) ->> 'id', 'UNKNOWN'),
        COALESCE ((T1.udf2 :: json) ->> 'task_id', 'UNKNOWN'),
        T1.requestor,
        UPPER((T1.udf2 :: json) ->> 'state'),
        T2.id,
        T3.id,
        T4.id,
        T5.id,
        ((T1.udf1 :: json) -> 'items' -> 0 ->> 'id'):: integer,
        ((T1.udf2 :: json) ->> 'estimated_price') :: double precision,
        T1.created_at,
        T1.updated_at
    FROM atlas_fmd_wrapper.search_request_bak_000 AS T1
        INNER JOIN atlas_fmd_wrapper.person AS T2 ON (T2.search_request_id_tmp = T1.id) AND (T2.is_start_tmp = true)
        INNER JOIN atlas_fmd_wrapper.person AS T3 ON (T3.search_request_id_tmp = T1.id) AND (T3.is_start_tmp = false)
        INNER JOIN atlas_fmd_wrapper.address AS T4 ON (T4.search_request_id_tmp = T1.id) AND (T4.is_start_tmp = true)
        INNER JOIN atlas_fmd_wrapper.address AS T5 ON (T5.search_request_id_tmp = T1.id) AND (T5.is_start_tmp = false);

ALTER TABLE atlas_fmd_wrapper.address DROP COLUMN search_request_id_tmp;

ALTER TABLE atlas_fmd_wrapper.address DROP COLUMN is_start_tmp;

ALTER TABLE atlas_fmd_wrapper.person DROP COLUMN search_request_id_tmp;

ALTER TABLE atlas_fmd_wrapper.person DROP COLUMN is_start_tmp;
