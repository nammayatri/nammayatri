CREATE TABLE atlas_app.cancellation_reason (
    reason_code character varying(255) PRIMARY KEY NOT NULL,
    description character varying(255) NOT NULL
);

CREATE TABLE atlas_app.ride_cancellation_reason (
    id character(36) PRIMARY KEY REFERENCES atlas_app.product_instance (id) on delete cascade,
    reason_code character varying(255) NOT NULL REFERENCES atlas_app.cancellation_reason (reason_code) on delete cascade,
    description character varying(255)
);

INSERT INTO atlas_app.cancellation_reason (reason_code, description) VALUES
  ('URGENT_BUSINESS', 'Urgent business came up.');

INSERT INTO atlas_app.cancellation_reason (reason_code, description) VALUES
  ('BAD_DRIVER', 'Something wrong with driver.');

INSERT INTO atlas_app.cancellation_reason (reason_code, description) VALUES
  ('DRIVER_IS_ABSENT', 'My driver is late.');

INSERT INTO atlas_app.cancellation_reason (reason_code, description) VALUES
  ('OTHER', 'Some other reason.');