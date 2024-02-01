CREATE TABLE atlas_driver_offer_bpp.picked_services ();

ALTER TABLE atlas_driver_offer_bpp.picked_services ADD COLUMN auto_complete text ;
ALTER TABLE atlas_driver_offer_bpp.picked_services ADD COLUMN get_distances text ;
ALTER TABLE atlas_driver_offer_bpp.picked_services ADD COLUMN get_distances_for_cancel_ride text ;
ALTER TABLE atlas_driver_offer_bpp.picked_services ADD COLUMN get_estimated_pickup_distances text ;
ALTER TABLE atlas_driver_offer_bpp.picked_services ADD COLUMN get_pickup_routes text ;
ALTER TABLE atlas_driver_offer_bpp.picked_services ADD COLUMN get_place_name text ;
ALTER TABLE atlas_driver_offer_bpp.picked_services ADD COLUMN get_routes text ;
ALTER TABLE atlas_driver_offer_bpp.picked_services ADD COLUMN get_trip_routes text ;
ALTER TABLE atlas_driver_offer_bpp.picked_services ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.picked_services ADD COLUMN search_request_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.picked_services ADD COLUMN snap_to_road text ;
ALTER TABLE atlas_driver_offer_bpp.picked_services ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.picked_services ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.picked_services ADD PRIMARY KEY ( search_request_id);