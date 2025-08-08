CREATE TABLE atlas_driver_offer_bpp.fleet_vehicle_assignment ();

ALTER TABLE atlas_driver_offer_bpp.fleet_vehicle_assignment ADD COLUMN amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fleet_vehicle_assignment ADD COLUMN assigned_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_vehicle_assignment ADD COLUMN assignment_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_vehicle_assignment ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_vehicle_assignment ADD COLUMN fleet_owner_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_vehicle_assignment ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_vehicle_assignment ADD COLUMN ticket_booking_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_vehicle_assignment ADD COLUMN ticket_booking_service_id text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_vehicle_assignment ADD COLUMN ticket_place_id text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_vehicle_assignment ADD COLUMN vehicle_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_vehicle_assignment ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_vehicle_assignment ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_vehicle_assignment ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_vehicle_assignment ADD COLUMN merchant_id character varying(36) ;