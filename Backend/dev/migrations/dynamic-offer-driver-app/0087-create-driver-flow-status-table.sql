CREATE TABLE atlas_driver_offer_bpp.driver_flow_status (
    person_id character(36) NOT NULL PRIMARY KEY REFERENCES atlas_driver_offer_bpp.person (id),
    flow_status JSON,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

INSERT INTO atlas_driver_offer_bpp.driver_flow_status (person_id, flow_status)
SELECT T1.id, '{"status":"IDLE"}'
FROM atlas_driver_offer_bpp.person as T1;

UPDATE atlas_driver_offer_bpp.driver_flow_status SET flow_status = '{"status":"ACTIVE"}' WHERE person_id in (SELECT driver_id FROM atlas_driver_offer_bpp.driver_information WHERE active);

ALTER TABLE atlas_driver_offer_bpp.driver_flow_status ALTER COLUMN flow_status SET NOT NULL;