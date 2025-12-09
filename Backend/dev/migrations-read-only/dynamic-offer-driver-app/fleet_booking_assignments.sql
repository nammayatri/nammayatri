CREATE TABLE atlas_driver_offer_bpp.fleet_booking_assignments ();

ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN booking_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN fleet_owner_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN main_assignment_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN place_name text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN service_id text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN service_name text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN vehicle_no text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD PRIMARY KEY ( id);
CREATE INDEX fleet_booking_assignments_idx_main_assignment_id ON atlas_driver_offer_bpp.fleet_booking_assignments USING btree (main_assignment_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN visit_date date ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN payment_method text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN sku_duration_mins integer ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN assignment_start_time timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_assignments ADD COLUMN assignment_end_time timestamp with time zone ;