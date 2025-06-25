CREATE TABLE atlas_app.frfs_ticket_booking ();

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN type text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN bpp_item_id text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN bpp_order_id text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN bpp_subscriber_id text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN bpp_subscriber_url text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN from_station_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN price double precision NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN provider_description text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN provider_id text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN provider_name text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN quantity integer NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN quote_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN search_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN stations_json text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN to_station_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_ticket_booking ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN payment_txn_id text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN bpp_bank_code text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN bpp_bank_account_number text ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN final_price double precision ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN estimated_price double precision NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN refund_amount double precision ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN cancellation_charges double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN is_booking_cancellable boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN currency text ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN customer_cancelled boolean NOT NULL default false;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN partner_org_transaction_id character varying(36) ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN partner_org_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ALTER COLUMN merchant_operating_city_id SET NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking ALTER COLUMN merchant_id SET NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN event_discount_amount double precision ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN discounted_tickets integer ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN payer_vpa text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN cashback_status text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN cashback_payout_order_id text ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN route_id character varying(36) ;

------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN service_tier_type text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN service_tier_short_name text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN service_tier_long_name text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN service_tier_description text ;

------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN service_tier_provider_code text ;
------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN bpp_delayed_interest integer ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN route_stations_json text ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking DROP COLUMN route_id;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN route_id text;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN discounts_json text ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN line_color text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN journey_leg_order integer ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN frequency integer ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN start_time timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN journey_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN journey_on_init_done boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN is_deleted boolean ;



------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN line_color_code text ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN journey_leg_status text ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN google_wallet_jwt_url text ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN platform_number text ;





------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN is_skipped boolean ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN is_fare_changed boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN integrated_bpp_config_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN recent_location_id character varying(36) ;


------- SQL updates -------


ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN os_type text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN os_build_version text ;
ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN booking_auth_code text ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking ADD COLUMN child_ticket_quantity integer ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

