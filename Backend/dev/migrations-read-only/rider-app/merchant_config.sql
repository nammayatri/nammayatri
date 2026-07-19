CREATE TABLE atlas_app.merchant_config ();

ALTER TABLE atlas_app.merchant_config ADD COLUMN created_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_config ADD COLUMN enabled boolean NOT NULL default True;
ALTER TABLE atlas_app.merchant_config ADD COLUMN fraud_booking_cancellation_count_threshold integer NOT NULL;
ALTER TABLE atlas_app.merchant_config ADD COLUMN fraud_booking_cancellation_count_window json NOT NULL default '{"period":24, "periodType":"Hours"}' ;
ALTER TABLE atlas_app.merchant_config ADD COLUMN fraud_booking_cancelled_by_driver_count_threshold integer NOT NULL default 5;
ALTER TABLE atlas_app.merchant_config ADD COLUMN fraud_booking_cancelled_by_driver_count_window json NOT NULL default '{"period":24, "periodType":"Hours"}';
ALTER TABLE atlas_app.merchant_config ADD COLUMN fraud_booking_total_count_threshold integer NOT NULL;
ALTER TABLE atlas_app.merchant_config ADD COLUMN fraud_ride_count_threshold integer NOT NULL default 0;
ALTER TABLE atlas_app.merchant_config ADD COLUMN fraud_ride_count_window json NOT NULL default '{"period":24, "periodType":"Hours"}';
ALTER TABLE atlas_app.merchant_config ADD COLUMN fraud_search_count_threshold integer NOT NULL default 5;
ALTER TABLE atlas_app.merchant_config ADD COLUMN fraud_search_count_window json NOT NULL default '{"period":24, "periodType":"Hours"}';
ALTER TABLE atlas_app.merchant_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_config ADD COLUMN updated_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_config ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.merchant_config ADD COLUMN fraud_auth_count_window json  default '{"period":20, "periodType":"Minutes"}';
ALTER TABLE atlas_app.merchant_config ADD COLUMN fraud_auth_count_threshold integer  default 8;

-- phone-number auth sliding-window rate limit (two independent windows) --
ALTER TABLE atlas_app.merchant_config ADD COLUMN auth_phone_number_count_threshold1 integer;
ALTER TABLE atlas_app.merchant_config ADD COLUMN auth_phone_number_count_window1 json;
ALTER TABLE atlas_app.merchant_config ADD COLUMN auth_phone_number_count_threshold2 integer;
ALTER TABLE atlas_app.merchant_config ADD COLUMN auth_phone_number_count_window2 json;
