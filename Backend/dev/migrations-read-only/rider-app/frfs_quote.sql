CREATE TABLE atlas_app.frfs_quote ();

ALTER TABLE atlas_app.frfs_quote ADD COLUMN type text NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN bpp_item_id text NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN bpp_subscriber_id text NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN bpp_subscriber_url text NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN from_station_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN price double precision NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN provider_description text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN provider_id text NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN provider_name text NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN quantity integer NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN search_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN stations_json text NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN to_station_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_quote ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.frfs_quote ADD COLUMN currency text ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_quote ADD COLUMN partner_org_transaction_id character varying(36) ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN partner_org_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_quote ALTER COLUMN merchant_operating_city_id SET NOT NULL;
ALTER TABLE atlas_app.frfs_quote ALTER COLUMN merchant_id SET NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_quote ADD COLUMN event_discount_amount double precision ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN discounted_tickets integer ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_quote ADD COLUMN route_id character varying(36) ;

------- SQL updates -------

ALTER TABLE atlas_app.frfs_quote ADD COLUMN service_tier_type text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN service_tier_short_name text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN service_tier_long_name text ;
ALTER TABLE atlas_app.frfs_quote ADD COLUMN service_tier_description text ;



------- SQL updates -------

ALTER TABLE atlas_app.frfs_quote ADD COLUMN service_tier_provider_code text ;

------- SQL updates -------

ALTER TABLE atlas_app.frfs_quote ADD COLUMN bpp_delayed_interest integer ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_quote ADD COLUMN route_stations_json text ;



------- SQL updates -------

ALTER TABLE atlas_app.frfs_quote ADD COLUMN discounts_json text ;

