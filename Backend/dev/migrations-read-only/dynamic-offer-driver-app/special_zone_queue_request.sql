CREATE TABLE atlas_driver_offer_bpp.special_zone_queue_request ();

ALTER TABLE atlas_driver_offer_bpp.special_zone_queue_request ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.special_zone_queue_request ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.special_zone_queue_request ADD COLUMN gate_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.special_zone_queue_request ADD COLUMN gate_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.special_zone_queue_request ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.special_zone_queue_request ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.special_zone_queue_request ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.special_zone_queue_request ADD COLUMN response text ;
ALTER TABLE atlas_driver_offer_bpp.special_zone_queue_request ADD COLUMN special_location_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.special_zone_queue_request ADD COLUMN special_location_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.special_zone_queue_request ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.special_zone_queue_request ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.special_zone_queue_request ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.special_zone_queue_request ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.special_zone_queue_request ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.special_zone_queue_request ADD COLUMN arrival_deadline_time timestamp with time zone ;