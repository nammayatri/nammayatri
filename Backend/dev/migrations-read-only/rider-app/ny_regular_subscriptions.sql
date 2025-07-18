CREATE TABLE atlas_app.ny_regular_subscriptions ();

ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN bpp_id text NOT NULL;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN dropoff_location_id character(36) NOT NULL;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN fixed_price_amount double precision ;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN fixed_price_currency text ;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN fixed_price_breakup_details json ;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN fixed_price_expiry_date timestamp with time zone ;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN id character(36) NOT NULL;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN initial_bpp_quote_id text ;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN metadata json ;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN pause_end_date timestamp with time zone ;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN pause_start_date timestamp with time zone ;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN pickup_location_id character(36) NOT NULL;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN recurrence_end_date date ;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN recurrence_rule_days text[] NOT NULL;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN scheduled_time_of_day time NOT NULL;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN start_datetime timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN status text NOT NULL default 'NEW';
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN user_id character(36) NOT NULL;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN vehicle_service_tier text ;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.ny_regular_subscriptions ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN fixed_price double precision ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN last_processed_at timestamp with time zone ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.ny_regular_subscriptions ADD COLUMN scheduling_hash text ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

