CREATE TABLE atlas_transporter.fare_policy_discount (
    id character(36) PRIMARY KEY NOT NULL,
    vehicle_variant character varying(255) NOT NULL,
    organization_id character(36) NOT NULL REFERENCES atlas_transporter.organization (id) ON DELETE CASCADE,
    start_time time NOT NULL,
    end_time time NOT NULL,
    discount double precision NOT NULL,
    enabled bool NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_transporter.discount_transaction (
    ride_booking_id character(36) PRIMARY KEY REFERENCES atlas_transporter.product_instance (id) ON DELETE CASCADE,
    organization_id character(36) NOT NULL REFERENCES atlas_transporter.organization (id) ON DELETE CASCADE,
    discount double precision NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_transporter.product_instance
  ADD COLUMN discount double precision;

ALTER TABLE atlas_transporter.product_instance
  ADD COLUMN estimated_total_fare numeric(30,2);

ALTER TABLE atlas_transporter.product_instance
  ADD COLUMN total_fare numeric(30,2);

UPDATE atlas_transporter.product_instance AS T1
  SET estimated_total_fare =
    ( SELECT T1.price
        FROM atlas_transporter.product_instance AS T2
        WHERE T1.id = T2.id
    );

ALTER TABLE atlas_transporter.product_instance ALTER COLUMN estimated_total_fare SET NOT NULL;