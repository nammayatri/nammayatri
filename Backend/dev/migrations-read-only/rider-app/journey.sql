CREATE TABLE atlas_app.journey ();

ALTER TABLE atlas_app.journey ADD COLUMN convenience_cost integer NOT NULL;
ALTER TABLE atlas_app.journey ADD COLUMN distance_unit character varying(255) NOT NULL;
ALTER TABLE atlas_app.journey ADD COLUMN estimated_distance double precision NOT NULL;
ALTER TABLE atlas_app.journey ADD COLUMN estimated_duration integer ;
ALTER TABLE atlas_app.journey ADD COLUMN estimated_fare double precision ;
ALTER TABLE atlas_app.journey ADD COLUMN currency text ;
ALTER TABLE atlas_app.journey ADD COLUMN fare double precision ;
ALTER TABLE atlas_app.journey ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey ADD COLUMN legs_done integer NOT NULL;
ALTER TABLE atlas_app.journey ADD COLUMN modes text[] NOT NULL;
ALTER TABLE atlas_app.journey ADD COLUMN search_request_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey ADD COLUMN total_legs integer NOT NULL;
ALTER TABLE atlas_app.journey ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.journey ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.journey ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.journey ADD COLUMN estimated_min_fare double precision ;
ALTER TABLE atlas_app.journey ADD COLUMN estimated_max_fare double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.journey ADD COLUMN start_time timestamp with time zone ;
ALTER TABLE atlas_app.journey ADD COLUMN end_time timestamp with time zone ;


------- SQL updates -------


--- Now DSL don't allow dropping tables instead we will drop not null constraint if any .Please be careful while running ---
ALTER TABLE atlas_app.journey ALTER COLUMN legs_done DROP NOT NULL;
--- Drop section ends. Please check before running ---




------- SQL updates -------
ALTER TABLE atlas_app.journey ADD COLUMN status text ;



------- SQL updates -------
ALTER TABLE atlas_app.journey ADD COLUMN rider_id character varying(36) NOT NULL default '';

ALTER TABLE atlas_app.journey ADD COLUMN is_payment_success boolean ;


------- SQL updates -------









------- SQL updates -------

ALTER TABLE atlas_app.journey ADD COLUMN recent_journal_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_app.journey ADD COLUMN recent_location_id character varying(36) ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.journey ADD COLUMN relevance_score double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.journey ADD COLUMN has_preferred_service_tier boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.journey ADD COLUMN has_preferred_transit_modes boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.journey ADD COLUMN to_location_address text ;
ALTER TABLE atlas_app.journey ADD COLUMN from_location_address text ;



------- SQL updates -------

ALTER TABLE atlas_app.journey ADD COLUMN is_public_transport_included boolean ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.journey ADD COLUMN payment_order_short_id character varying(36) ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.journey ADD COLUMN journey_expiry_time timestamp with time zone ;



------- SQL updates -------

ALTER TABLE atlas_app.journey ADD COLUMN to_location_id character varying(36) ;
ALTER TABLE atlas_app.journey ADD COLUMN from_location_id character varying(36) ;


------- SQL updates -------


ALTER TABLE atlas_app.journey ALTER COLUMN search_request_id TYPE text;




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

ALTER TABLE atlas_app.journey ADD COLUMN has_started_tracking_without_booking boolean ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------



------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.journey ADD COLUMN is_single_mode boolean ;