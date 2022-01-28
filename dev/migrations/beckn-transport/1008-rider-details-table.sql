ALTER TABLE atlas_transporter.search_request DROP COLUMN requestor_id;

ALTER TABLE atlas_transporter.ride_booking RENAME COLUMN requestor_id TO rider_id;
ALTER TABLE atlas_transporter.ride_booking DROP COLUMN requestor_mobile_number;

CREATE TABLE atlas_transporter.rider_details (
    id character(36) PRIMARY KEY NOT NULL,
    mobile_country_code character varying(255) NOT NULL,
    mobile_number_encrypted character varying(255) NOT NULL,
    mobile_number_hash bytea,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ride_details_unique_mobile_number UNIQUE (mobile_number_hash, mobile_country_code)
);

INSERT INTO atlas_transporter.rider_details (id, mobile_country_code, mobile_number_encrypted, mobile_number_hash) VALUES
  ('UNKNOWN', 'UNKNOWN', 'UNKNOWN', '');

UPDATE atlas_transporter.ride_booking SET rider_id = 'UNKNOWN';

ALTER TABLE atlas_transporter.ride_booking
    ADD CONSTRAINT ride_booking_rider_id_fkey FOREIGN KEY (rider_id)
        REFERENCES atlas_transporter.rider_details (id);