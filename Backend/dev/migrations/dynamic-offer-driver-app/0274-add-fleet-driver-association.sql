CREATE TABLE fleet_driver_association(
    id TEXT PRIMARY KEY,
    driver_id TEXT NOT NULL,
    fleet_owner_id TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL
);


ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD COLUMN fleet_owner_id character (36);