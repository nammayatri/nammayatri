CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.fleet_driver_association(
    id TEXT PRIMARY KEY,
    driver_id TEXT NOT NULL,
    fleet_owner_id TEXT NOT NULL,
    is_active BOOLEAN NOT NULL ,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD COLUMN fleet_owner_id character (36);
ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN fleet_owner_id CHARACTER VARYING(36);
CREATE INDEX idx_vehicle_registration_certificate_fleet_owner_id ON atlas_driver_offer_bpp.vehicle_registration_certificate USING btree (fleet_owner_id);
CREATE INDEX idx_ride_detail_fleet_owner_id ON atlas_driver_offer_bpp.ride_details USING btree (fleet_owner_id);
CREATE INDEX idx_ride_detail_driver_number_hash ON atlas_driver_offer_bpp.ride_details USING btree (driver_number_hash);
CREATE INDEX idx_ride_detail_vehicle_number ON atlas_driver_offer_bpp.ride_details USING btree (vehicle_number);
----- DROP COLUMN -----

ALTER TABLE atlas_driver_offer_bpp.vehicle DROP COLUMN IF EXISTS fleet_owner_id;