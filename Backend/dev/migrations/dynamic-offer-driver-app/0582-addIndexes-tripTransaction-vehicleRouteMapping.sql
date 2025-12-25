CREATE INDEX idx_vehicle_number_hashed_vrm ON atlas_driver_offer_bpp.vehicle_route_mapping USING btree (vehicle_number_hash);

CREATE INDEX idx_driver_id_trip_transaction ON atlas_driver_offer_bpp.trip_transaction USING btree (driver_id);