CREATE TABLE atlas_transporter.cancellation_reason (
    reason_code character varying(255) PRIMARY KEY NOT NULL,
    description character varying(255) NOT NULL
);

CREATE TABLE atlas_transporter.ride_cancellation_reason (
    id character(36) PRIMARY KEY REFERENCES atlas_transporter.product_instance (id) on delete cascade,
    reason_code character varying(255) NOT NULL REFERENCES atlas_transporter.cancellation_reason (reason_code) on delete cascade,
    description character varying(255)
);

INSERT INTO atlas_transporter.cancellation_reason (reason_code, description) VALUES
  ('CRASH', 'I had an accident.');

INSERT INTO atlas_transporter.cancellation_reason (reason_code, description) VALUES
  ('CAR_IS_MALFUNCTIONING', 'Car is malfunctioning');

INSERT INTO atlas_transporter.cancellation_reason (reason_code, description) VALUES
  ('REQUESTOR_IS_ABSENT', 'I can not find my rider.');

INSERT INTO atlas_transporter.cancellation_reason (reason_code, description) VALUES
  ('OTHER', 'Some other reason.');