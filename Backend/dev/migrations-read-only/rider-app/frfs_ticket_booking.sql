CREATE TABLE atlas_app.frfs_ticket_booking ();

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN type text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN booking_auth_code text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN bpp_bank_account_number text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN bpp_bank_code text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN bpp_delayed_interest integer ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN bpp_item_id text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN bpp_order_id text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN bpp_subscriber_id text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN bpp_subscriber_url text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN cancellation_charges double precision ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN cashback_payout_order_id text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN cashback_status text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN child_ticket_quantity integer ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN customer_cancelled boolean NOT NULL default false;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN discounted_tickets integer ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN discounts_json text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN estimated_price double precision NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN event_discount_amount double precision ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN final_price double precision ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN from_station_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN google_wallet_jwt_url text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN integrated_bpp_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN is_booking_cancellable boolean ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN is_deleted boolean ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN is_fare_changed boolean ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN is_skipped boolean ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN journey_id character varying(36) ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN journey_leg_order integer ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN journey_leg_status text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN journey_on_init_done boolean ;

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN os_build_version text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN os_type text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN partner_org_id character varying(36) ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN partner_org_transaction_id character varying(36) ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN payer_vpa text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN payment_txn_id text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN currency text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN price double precision NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN provider_description text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN provider_id text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN provider_name text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN quantity integer NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN quote_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN recent_location_id character varying(36) ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN refund_amount double precision ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN route_stations_json text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN search_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN start_time timestamp with time zone ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN stations_json text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN to_station_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_ticket_booking ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ALTER COLUMN to_station_id TYPE text;
ALTER TABLE atlas_app.frfs_ticket_booking ALTER COLUMN from_station_id TYPE text;



------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN journey_leg_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN multimodal_search_request_id text ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN failure_reason text ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN is_single_mode boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ALTER COLUMN quantity SET DEFAULT 0;
ALTER TABLE atlas_app.frfs_ticket_booking ALTER COLUMN quantity DROP NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ALTER COLUMN estimated_price SET DEFAULT 0;
ALTER TABLE atlas_app.frfs_ticket_booking ALTER COLUMN estimated_price DROP NOT NULL;



------- SQL updates -------


ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN to_station_point text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN to_station_name text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN to_station_address text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN from_station_point text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN from_station_name text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN from_station_address text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN bus_location_data json ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN to_station_lon double precision ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN to_station_lat double precision ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN from_station_lon double precision ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN from_station_lat double precision ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN vehicle_number text ;



------- SQL updates -------

