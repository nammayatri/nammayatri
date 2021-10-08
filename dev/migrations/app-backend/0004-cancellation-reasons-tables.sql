CREATE TABLE atlas_app.cancellation_reason (
    reason_code character varying(255) PRIMARY KEY NOT NULL,
    description character varying(255) NOT NULL,
    enabled bool NOT NULL
);

CREATE TABLE atlas_app.ride_cancellation_reason (
    ride_id character(36) PRIMARY KEY REFERENCES atlas_app.product_instance (id) on delete cascade,
    source character varying(255) NOT NULL,
    reason_code character varying(255),
    additional_info character varying(255)
);

INSERT INTO atlas_app.cancellation_reason (reason_code, description, enabled) VALUES
  ('COULDNOT_CONNECT_WITH_DRIVER', 'I could not connect to driver.', true);

INSERT INTO atlas_app.cancellation_reason (reason_code, description, enabled) VALUES
  ('HIGH_FARE', 'My fare was too high.', true);

INSERT INTO atlas_app.cancellation_reason (reason_code, description, enabled) VALUES
  ('WRONG_PICKUP_LOC', 'The pickup location was incorrect.', true);

INSERT INTO atlas_app.cancellation_reason (reason_code, description, enabled) VALUES
  ('FORCED_BY_DRIVER', 'Driver requested me to cancel.', true);

INSERT INTO atlas_app.cancellation_reason (reason_code, description, enabled) VALUES
  ('LONG_ETA', 'ETA was too long.', true);

INSERT INTO atlas_app.cancellation_reason (reason_code, description, enabled) VALUES
  ('SHORT_ETA', 'ETA was too short.', true);

INSERT INTO atlas_app.cancellation_reason (reason_code, description, enabled) VALUES
  ('OTHER', 'Some other reason.', true);