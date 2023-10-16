ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN fleet_owner_id CHARACTER VARYING(36);
CREATE INDEX idx_vehicle_fleet_owner_id ON atlas_driver_offer_bpp.vehicle USING btree (fleet_owner_id);