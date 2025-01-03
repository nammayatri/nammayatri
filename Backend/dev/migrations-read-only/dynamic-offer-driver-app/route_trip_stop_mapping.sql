CREATE TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ();

ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN provider_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN route_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN scheduled_arrival time without time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN scheduled_day date NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN scheduled_departure time without time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN stop_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN stop_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN stop_lat double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN stop_lon double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN stop_sequence_num integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN trip_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN trip_sequence_num integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ADD PRIMARY KEY ( route_code, scheduled_day, stop_code, trip_code, trip_sequence_num);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.route_trip_stop_mapping ALTER COLUMN scheduled_day TYPE text;