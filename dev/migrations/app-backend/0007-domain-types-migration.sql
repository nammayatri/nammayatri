UPDATE atlas_app.product_instance AS T1 
	SET case_id = (SELECT case_id FROM atlas_app.product_instance AS T2 WHERE T2.id = T1.parent_id)
	WHERE T1.type <> 'RIDESEARCH';

DELETE FROM atlas_app."case" AS T1 WHERE T1.type <> 'RIDESEARCH';

ALTER TABLE atlas_app."case" RENAME TO search_request;

ALTER TABLE atlas_app.product_instance RENAME COLUMN case_id TO request_id;

ALTER TABLE atlas_app.search_request DROP COLUMN parent_case_id;

CREATE TABLE atlas_app.ride (
    id character(36) NOT NULL,
    request_id character varying(255) NOT NULL,
    product_id character varying(255) NOT NULL,
    person_id character varying(255),
    person_updated_at timestamp with time zone,
    short_id character varying(36) NOT NULL,
    entity_id character varying(255),
    entity_type character varying(255) NOT NULL,
    quantity bigint NOT NULL,
    price numeric(30,10),
    actual_price double precision,
    status character varying(255) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    end_time timestamp with time zone,
    valid_till timestamp with time zone NOT NULL,
    from_location_id character varying(255),
    to_location_id character varying(255),
    organization_id character varying(255) NOT NULL,
    product_instance_id character varying(255),
    actual_distance double precision,
    info text,
    udf1 character varying(255),
    udf2 character varying(255),
    udf3 character varying(255),
    udf4 character varying(255),
    udf5 character varying(255),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_app.ride OWNER TO atlas;

ALTER TABLE ONLY atlas_app.ride
    ADD CONSTRAINT idx_16395_primary PRIMARY KEY (id);

CREATE INDEX idx_16395_case_id ON atlas_app.ride USING btree (request_id);

CREATE INDEX idx_16395_entity_id ON atlas_app.ride USING btree (entity_id);

CREATE INDEX idx_16395_organization_id ON atlas_app.ride USING btree (organization_id);

CREATE INDEX idx_16395_product_instance_id ON atlas_app.ride USING btree (product_instance_id);

CREATE INDEX idx_16395_person_id ON atlas_app.ride USING btree (person_id);

CREATE INDEX idx_16395_product_id ON atlas_app.ride USING btree (product_id);

CREATE INDEX idx_16395_status ON atlas_app.ride USING btree (status);

INSERT INTO atlas_app.ride 
    SELECT id,
    request_id,
    product_id,
    person_id,
    person_updated_at,
    short_id,
    entity_id,
    entity_type,
    quantity,
    price,
    actual_price,
    status,
    start_time,
    end_time,
    valid_till,
    from_location_id,
    to_location_id,
    organization_id,
    parent_id AS product_instance_id,
    actual_distance,
    info,
    udf1,
    udf2,
    udf3,
    udf4,
    udf5,
    created_at ,
    updated_at FROM atlas_app.product_instance AS T1
        WHERE T1.type = 'RIDEORDER';

ALTER TABLE atlas_app.ride_cancellation_reason
   DROP CONSTRAINT ride_cancellation_reason_ride_id_fkey
 , ADD  CONSTRAINT ride_cancellation_reason_ride_id_fkey FOREIGN KEY (ride_id)
      REFERENCES atlas_app.ride (id) on delete cascade;

DELETE FROM atlas_app.product_instance AS T1 WHERE T1.type = 'RIDEORDER';

ALTER TABLE atlas_app.search_request DROP COLUMN type;

ALTER TABLE atlas_app.product_instance DROP COLUMN type;

ALTER TABLE atlas_app.product_instance DROP COLUMN parent_id;

ALTER TABLE atlas_app.product_instance RENAME TO quote;

ALTER TABLE atlas_app.ride RENAME COLUMN product_instance_id TO quote_id;

ALTER TABLE atlas_app.issues RENAME COLUMN product_instance_id TO ride_booking_id;

ALTER TABLE atlas_app.quote DROP COLUMN product_id;

ALTER TABLE atlas_app.ride DROP COLUMN product_id;

DROP TABLE atlas_app.product;