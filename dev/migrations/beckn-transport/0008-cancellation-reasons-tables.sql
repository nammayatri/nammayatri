CREATE TABLE atlas_transporter.cancellation_reason (
    reason_code character varying(255) NOT NULL,
    description character varying(255) NOT NULL
);

CREATE TABLE atlas_transporter.ride_cancellation_reason (
    id character(36) PRIMARY KEY REFERENCES atlas_transporter.product_instance (id) on delete cascade,
    reason_code character varying(255) NOT NULL REFERENCES atlas_transporter.cancellation_reasons (reason_code) on delete cascade,
    description character varying(255)
);