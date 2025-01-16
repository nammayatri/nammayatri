CREATE TABLE atlas_driver_offer_bpp.booking_update_request ();

ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN bap_booking_update_request_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN booking_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN current_point_lat double precision ;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN current_point_lon double precision ;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN estimated_distance integer ;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN estimated_fare double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN fare_params_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN fare_policy_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN max_estimated_distance integer ;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN old_estimated_distance integer ;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN old_estimated_fare double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN old_fare_params_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN old_max_estimated_distance integer ;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN total_distance integer ;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN travelled_distance integer ;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN distance_unit character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN route_info_resp text ;
ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN get_route_req text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking_update_request ADD COLUMN snap_to_road_failed boolean ;