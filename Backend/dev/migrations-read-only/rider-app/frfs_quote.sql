CREATE TABLE atlas_app.frfs_quote ();

ALTER TABLE atlas_app.frfs_quote ADD COLUMN type text NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN bpp_delayed_interest integer ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN bpp_item_id text NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN bpp_subscriber_id text NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN bpp_subscriber_url text NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN child_price double precision ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN child_ticket_quantity integer ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN discounted_tickets integer ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN discounts_json text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN estimated_price double precision ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN event_discount_amount double precision ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN app_session integer ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN distance integer ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN provider_route_id text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN sdk_token text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN ticket_type_code text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN train_type_code text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN via text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN from_station_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN integrated_bpp_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN old_cache_dump text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN partner_org_id character varying(36) ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN partner_org_transaction_id character varying(36) ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN currency text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN price double precision NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN provider_description text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN provider_id text NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN provider_name text NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN quantity integer NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN route_stations_json text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN search_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN stations_json text NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN to_station_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_quote ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.frfs_quote ALTER COLUMN to_station_id TYPE text;
ALTER TABLE atlas_app.frfs_quote ALTER COLUMN from_station_id TYPE text;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_quote ADD COLUMN multimodal_search_request_id text ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_quote ALTER COLUMN quantity SET DEFAULT 0;
ALTER TABLE atlas_app.frfs_quote ALTER COLUMN quantity DROP NOT NULL;
ALTER TABLE atlas_app.frfs_quote ALTER COLUMN price SET DEFAULT 0;
ALTER TABLE atlas_app.frfs_quote ALTER COLUMN price DROP NOT NULL;
ALTER TABLE atlas_app.frfs_quote ALTER COLUMN estimated_price SET DEFAULT 0;
ALTER TABLE atlas_app.frfs_quote ALTER COLUMN child_price SET DEFAULT 0;



------- SQL updates -------

ALTER TABLE atlas_app.frfs_quote ADD COLUMN to_station_name text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN to_station_lon double precision ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN to_station_lat double precision ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN to_station_address text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN from_station_name text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN from_station_lon double precision ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN from_station_lat double precision ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN from_station_address text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN bus_location_data json ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN vehicle_number text ;



------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

