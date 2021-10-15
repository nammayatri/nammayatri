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
    discount double precision,
    estimated_total_fare numeric(30,2),
    total_fare numeric(30,2),
    status character varying(255) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    end_time timestamp with time zone,
    valid_till timestamp with time zone NOT NULL,
    from_location_id character varying(255),
    to_location_id character varying(255),
    organization_id character varying(255) NOT NULL,
    product_instance_id character varying(255),
    chargeable_distance double precision,
    info text,
    vehicle_variant character varying(60) NOT NULL,
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
    discount,
    estimated_total_fare,
    total_fare,
    status,
    start_time,
    end_time,
    valid_till,
    from_location_id,
    to_location_id,
    organization_id,
    parent_id AS product_instance_id,
    chargeable_distance,
    info,
    vehicle_variant,
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

ALTER TABLE atlas_app.search_request DROP COLUMN name;
ALTER TABLE atlas_app.search_request DROP COLUMN description;
ALTER TABLE atlas_app.search_request DROP COLUMN short_id;
ALTER TABLE atlas_app.search_request DROP COLUMN status;
ALTER TABLE atlas_app.search_request DROP COLUMN industry;
ALTER TABLE atlas_app.search_request DROP COLUMN end_time;
ALTER TABLE atlas_app.search_request DROP COLUMN exchange_type;
ALTER TABLE atlas_app.search_request DROP COLUMN provider;
ALTER TABLE atlas_app.search_request DROP COLUMN provider_type;
ALTER TABLE atlas_app.search_request DROP COLUMN requestor_type;
ALTER TABLE atlas_app.search_request DROP COLUMN udf1;
ALTER TABLE atlas_app.search_request DROP COLUMN udf2;
ALTER TABLE atlas_app.search_request DROP COLUMN udf3;
ALTER TABLE atlas_app.search_request DROP COLUMN udf4;
ALTER TABLE atlas_app.search_request DROP COLUMN info;
ALTER TABLE atlas_app.search_request DROP COLUMN updated_at;

ALTER TABLE atlas_app.search_request RENAME COLUMN udf5 TO distance;
ALTER TABLE atlas_app.search_request ALTER COLUMN distance TYPE double precision USING (distance :: double precision);
UPDATE atlas_app.search_request AS T1
	SET distance = 0 WHERE distance IS NULL;
ALTER TABLE atlas_app.search_request ALTER COLUMN distance SET NOT NULL;

ALTER TABLE atlas_app.search_request RENAME COLUMN requestor TO requestor_id;
UPDATE atlas_app.search_request AS T1
	SET requestor_id = 'UNKNOWN' WHERE requestor_id IS NULL;
ALTER TABLE atlas_app.search_request ALTER COLUMN requestor_id SET NOT NULL;

ALTER TABLE atlas_app.ride RENAME TO old_ride;

UPDATE atlas_app.old_ride AS T1
	SET udf4 = (SELECT udf4 FROM atlas_app.quote AS T2 WHERE T1.quote_id = T2.id);

CREATE TABLE atlas_app.ride_booking (
    id character(36) PRIMARY KEY NOT NULL,
    request_id character(36) NOT NULL REFERENCES atlas_app.search_request (id) on delete cascade,
    quote_id character(36) NOT NULL REFERENCES atlas_app.quote (id) on delete cascade,
    status character varying(255) NOT NULL,
    provider_id character(36) NOT NULL REFERENCES atlas_app.organization (id) on delete cascade,
    provider_mobile_number character varying(255) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    requestor_id character(36) NOT NULL,
    from_location_id character(36) NOT NULL REFERENCES atlas_app.search_request_location (id) on delete cascade,
    to_location_id character(36) NOT NULL REFERENCES atlas_app.search_request_location (id) on delete cascade,
    price double precision NOT NULL,
    discount double precision,
    estimated_total_fare numeric(30,2) NOT NULL,
    distance double precision NOT NULL,
    vehicle_variant character varying(60) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_app.ride (
    id character(36) PRIMARY KEY NOT NULL,
    booking_id character(36) NOT NULL REFERENCES atlas_app.ride_booking (id) on delete cascade,
    short_id character varying(36) NOT NULL,
    status character varying(255) NOT NULL,
    driver_name character varying(255) NOT NULL,
    driver_rating double precision,
    driver_mobile_number character varying(255) NOT NULL,
    driver_registered_at timestamp with time zone NOT NULL,
    vehicle_number character varying(255) NOT NULL,
    vehicle_model character varying(255) NOT NULL,
    vehicle_color character varying(255) NOT NULL,
    otp character(4) NOT NULL,
    tracking_url character varying(255) NOT NULL,
    final_price double precision,
    total_fare numeric(30,2),
    chargeable_distance double precision,
    vehicle_variant character varying(60) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_app.quote ALTER COLUMN price SET NOT NULL;
ALTER TABLE atlas_app.quote RENAME COLUMN organization_id TO provider_id;
ALTER TABLE atlas_app.quote ADD COLUMN provider_mobile_number character varying(255);
ALTER TABLE atlas_app.quote ADD COLUMN distance_to_nearest_driver float;

UPDATE atlas_app.quote AS T1
	SET distance_to_nearest_driver = CAST (udf1 AS float);
UPDATE atlas_app.quote AS T1
	SET distance_to_nearest_driver = 0 WHERE distance_to_nearest_driver IS NULL;

UPDATE atlas_app.quote AS T1
	SET provider_mobile_number = (SELECT T2.mobile_number FROM atlas_app.organization AS T2 WHERE T2.id = T1.provider_id);
UPDATE atlas_app.quote AS T1
	SET provider_mobile_number = 'UNKNOWN' WHERE provider_mobile_number IS NULL;

ALTER TABLE atlas_app.quote ALTER COLUMN provider_mobile_number SET NOT NULL;
ALTER TABLE atlas_app.quote ALTER COLUMN distance_to_nearest_driver SET NOT NULL;
ALTER TABLE atlas_app.quote ALTER COLUMN price SET NOT NULL;

INSERT INTO atlas_app.ride_booking
    SELECT
        T1.id,
        T1.request_id,
        T1.quote_id,
        T1.status,
        T1.organization_id,
        COALESCE ((T1.info :: json) -> 'provider' -> 'phones' ->> 0, 'UNKNOWN'),
        T2.start_time,
        T2.requestor_id,
        T1.from_location_id,
        T1.to_location_id,
        T1.price,
        T1.discount,
        T1.estimated_total_fare,
        T2.distance :: double precision,
        T1.vehicle_variant,
        T1.created_at,
        T1.updated_at
    FROM atlas_app.old_ride AS T1
    JOIN atlas_app.search_request AS T2
        ON T1.request_id = T2.id;

UPDATE atlas_app.ride_booking AS T1
	SET status = 'TRIP_ASSIGNED' WHERE T1.status = 'INPROGRESS';

INSERT INTO atlas_app.ride
    SELECT
        T1.id,
        T1.id,
        T1.short_id,
        T1.status,
        COALESCE ((T1.info :: json)  -> 'tracker' -> 'trip' -> 'driver' ->> 'name', 'UNKNOWN'),
        ((T1.info :: json)  -> 'tracker' -> 'trip' -> 'driver' ->> 'rating') :: double precision,
        COALESCE ((T1.info :: json)  -> 'tracker' -> 'trip' -> 'driver' -> 'phones' ->> 0, 'UNKNOWN'),
        COALESCE (((T1.info :: json)  -> 'tracker' -> 'trip' -> 'driver' ->> 'registeredAt') :: timestamp with time zone, now ()),
        COALESCE ((T1.info :: json)  -> 'tracker' -> 'trip' -> 'vehicle' ->> 'registrationNumber', 'UNKNOWN'),
        COALESCE ((T1.info :: json)  -> 'tracker' -> 'trip' -> 'vehicle' ->> 'model', 'UNKNOWN'),
        COALESCE ((T1.info :: json)  -> 'tracker' -> 'trip' -> 'vehicle' ->> 'color', 'UNKNOWN'),
        T1.udf4,
        'UNKNOWN',
        T1.actual_price,
        T1.total_fare,
        T1.chargeable_distance,
        T1.vehicle_variant,
        T1.created_at,
        T1.updated_at
    FROM atlas_app.old_ride AS T1
    WHERE T1.status != 'CONFIRMED' AND ((T1.info :: json) -> 'tracker' -> 'trip' ->> 'driver') != 'null';

UPDATE atlas_app.ride AS T1
	SET status = 'NEW' WHERE T1.status = 'TRIP_ASSIGNED';

ALTER TABLE atlas_app.quote ADD COLUMN provider_name character varying(255);
ALTER TABLE atlas_app.quote ADD COLUMN provider_completed_rides_count integer;

UPDATE atlas_app.quote AS T1
	SET provider_name = COALESCE ((T1.info :: json) -> 'provider' ->> 'name', 'UNKNOWN');

UPDATE atlas_app.quote AS T1
	SET provider_completed_rides_count = COALESCE (((T1.info :: json) -> 'provider' -> 'info' ->> 'completed') :: integer, 0);

ALTER TABLE atlas_app.quote ALTER COLUMN provider_name SET NOT NULL;
ALTER TABLE atlas_app.quote ALTER COLUMN provider_completed_rides_count SET NOT NULL;

ALTER TABLE atlas_app.quote DROP COLUMN person_id;
ALTER TABLE atlas_app.quote DROP COLUMN person_updated_at;
ALTER TABLE atlas_app.quote DROP COLUMN short_id;
ALTER TABLE atlas_app.quote DROP COLUMN entity_id;
ALTER TABLE atlas_app.quote DROP COLUMN entity_type;
ALTER TABLE atlas_app.quote DROP COLUMN quantity;
ALTER TABLE atlas_app.quote DROP COLUMN status;
ALTER TABLE atlas_app.quote DROP COLUMN start_time;
ALTER TABLE atlas_app.quote DROP COLUMN end_time;
ALTER TABLE atlas_app.quote DROP COLUMN valid_till;
ALTER TABLE atlas_app.quote DROP COLUMN from_location_id;
ALTER TABLE atlas_app.quote DROP COLUMN to_location_id;
ALTER TABLE atlas_app.quote DROP COLUMN info;
ALTER TABLE atlas_app.quote DROP COLUMN udf1;
ALTER TABLE atlas_app.quote DROP COLUMN udf2;
ALTER TABLE atlas_app.quote DROP COLUMN udf3;
ALTER TABLE atlas_app.quote DROP COLUMN udf4;
ALTER TABLE atlas_app.quote DROP COLUMN udf5;
ALTER TABLE atlas_app.quote DROP COLUMN updated_at;
ALTER TABLE atlas_app.quote DROP COLUMN chargeable_distance;
ALTER TABLE atlas_app.quote DROP COLUMN actual_price;

ALTER TABLE atlas_app.ride_cancellation_reason RENAME COLUMN ride_id TO ride_booking_id;

ALTER TABLE atlas_app.ride_cancellation_reason
   DROP CONSTRAINT ride_cancellation_reason_ride_id_fkey
 , ADD  CONSTRAINT ride_cancellation_reason_ride_booking_id_fkey FOREIGN KEY (ride_booking_id)
      REFERENCES atlas_app.ride_booking (id) on delete cascade;

DROP TABLE atlas_app.old_ride;
