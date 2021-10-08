CREATE TABLE atlas_transporter.fare_policy_discount (
    id character(36) PRIMARY KEY NOT NULL,
    vehicle_variant character varying(255) NOT NULL,
    organization_id character(36) NOT NULL REFERENCES atlas_transporter.organization (id) ON DELETE CASCADE,
    start_time time NOT NULL,
    end_time time NOT NULL,
    discount double precision NOT NULL,
    enabled bool NOT NULL
);

CREATE TABLE atlas_transporter.discount_transaction (
    ride_booking_id character(36) PRIMARY KEY REFERENCES atlas_transporter.product_instance (id) ON DELETE CASCADE,
    organization_id character(36) NOT NULL REFERENCES atlas_transporter.organization (id) ON DELETE CASCADE,
    discount double precision NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_transporter.product_instance
  ADD COLUMN discount double precision;