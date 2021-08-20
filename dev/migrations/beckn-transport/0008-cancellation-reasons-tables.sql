CREATE TABLE atlas_transporter.cancellation_reason (
    reason_code character varying(255) PRIMARY KEY NOT NULL,
    description character varying(255) NOT NULL,
    enabled bool NOT NULL
);

CREATE TABLE atlas_transporter.ride_cancellation_reason (
    ride_id character(36) PRIMARY KEY REFERENCES atlas_transporter.product_instance (id) on delete cascade,
    source character varying(255) NOT NULL,
    reason_code character varying(255),
    additional_info character varying(255)
);

INSERT INTO atlas_transporter.cancellation_reason (reason_code, description, enabled) VALUES
  ('NOT_REACHABLE', 'Customer not reachable.', true);

INSERT INTO atlas_transporter.cancellation_reason (reason_code, description, enabled) VALUES
  ('VEHICLE_ISSUE', 'Vehicle issue.', true);

INSERT INTO atlas_transporter.cancellation_reason (reason_code, description, enabled) VALUES
  ('ACCEPTED_BY_MISSTAKE', 'Accepted by mistake.', true);

INSERT INTO atlas_transporter.cancellation_reason (reason_code, description, enabled) VALUES
  ('PICKUP_LOC_TOO_FAR', 'Pickup location was too far.', true);

INSERT INTO atlas_transporter.cancellation_reason (reason_code, description, enabled) VALUES
  ('COVID_RISK', 'COVID Risk. Customer not wearing mask/unwell.', true);

INSERT INTO atlas_transporter.cancellation_reason (reason_code, description, enabled) VALUES
  ('EXCESS_LUGGAGE', 'Excess luggage.', true);

INSERT INTO atlas_transporter.cancellation_reason (reason_code, description, enabled) VALUES
  ('TOO_MUCH_PASSANGERS', 'Passenger limit exceeded.', true);

INSERT INTO atlas_transporter.cancellation_reason (reason_code, description, enabled) VALUES
  ('OTHER', 'Some other reason.', true);