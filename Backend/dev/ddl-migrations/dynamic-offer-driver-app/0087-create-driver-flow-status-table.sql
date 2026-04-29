CREATE TABLE atlas_driver_offer_bpp.driver_flow_status (
    person_id character(36) NOT NULL PRIMARY KEY REFERENCES atlas_driver_offer_bpp.person (id),
    flow_status JSON,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_driver_offer_bpp.driver_flow_status ALTER COLUMN flow_status SET NOT NULL;