CREATE TABLE atlas_app.frfs_search ();

ALTER TABLE atlas_app.frfs_search ADD COLUMN from_station_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_search ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_search ADD COLUMN integrated_bpp_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_search ADD COLUMN is_on_search_received boolean ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN agency text ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN convenience_cost integer ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN is_deleted boolean ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN journey_id text ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN journey_leg_order integer ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN on_search_failed boolean ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN pricing_id text ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN skip_booking boolean ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN journey_leg_status text ;

ALTER TABLE atlas_app.frfs_search ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_search ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_search ADD COLUMN partner_org_id character varying(36) ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN partner_org_transaction_id character varying(36) ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN quantity integer NOT NULL;
ALTER TABLE atlas_app.frfs_search ADD COLUMN recent_location_id character varying(36) ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_search ADD COLUMN route_id text ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN to_station_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_search ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.frfs_search ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_search ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_search ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.frfs_search ALTER COLUMN to_station_id TYPE text;
ALTER TABLE atlas_app.frfs_search ALTER COLUMN from_station_id TYPE text;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_search ADD COLUMN valid_till timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_search ADD COLUMN journey_leg_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_search ALTER COLUMN journey_leg_id TYPE text;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_search ADD COLUMN multimodal_search_request_id text ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_search ADD COLUMN search_as_parent_stops boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_search ADD COLUMN to_station_lon double precision ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN to_station_lat double precision ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN from_station_lon double precision ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN from_station_lat double precision ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN to_station_name text ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN to_station_address text ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN from_station_name text ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN from_station_address text ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN bus_location_data json ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN vehicle_number text ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN minimal_data boolean ;



------- SQL updates -------

