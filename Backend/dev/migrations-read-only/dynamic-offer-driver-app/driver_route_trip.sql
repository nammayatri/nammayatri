CREATE TABLE atlas_driver_offer_bpp.driver_route_trip ();

ALTER TABLE atlas_driver_offer_bpp.driver_route_trip ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_route_trip ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_route_trip ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_route_trip ADD COLUMN idempotency_key text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_route_trip ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_route_trip ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_route_trip ADD COLUMN ride_id text ;
ALTER TABLE atlas_driver_offer_bpp.driver_route_trip ADD COLUMN route_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_route_trip ADD COLUMN source text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_route_trip ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_route_trip ADD COLUMN vehicle_number text ;
ALTER TABLE atlas_driver_offer_bpp.driver_route_trip ADD PRIMARY KEY ( id);
CREATE INDEX CONCURRENTLY driver_route_trip_idx_driver_id ON atlas_driver_offer_bpp.driver_route_trip USING btree (driver_id);
CREATE INDEX CONCURRENTLY driver_route_trip_idx_idempotency_key ON atlas_driver_offer_bpp.driver_route_trip USING btree (idempotency_key);
CREATE INDEX CONCURRENTLY driver_route_trip_idx_created_at_driver_id ON atlas_driver_offer_bpp.driver_route_trip USING btree (created_at, driver_id);