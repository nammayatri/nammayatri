CREATE TABLE atlas_app.shared_search_request ();

ALTER TABLE atlas_app.shared_search_request ADD COLUMN created_at timestamptz NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.shared_search_request ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.shared_search_request ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_app.shared_search_request ADD COLUMN max_distance double precision ;
ALTER TABLE atlas_app.shared_search_request ADD COLUMN max_distance_value double precision ;
ALTER TABLE atlas_app.shared_search_request ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.shared_search_request ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.shared_search_request ADD COLUMN search_request_ids uuid[] NOT NULL;
ALTER TABLE atlas_app.shared_search_request ADD COLUMN status character varying(255) NOT NULL;
ALTER TABLE atlas_app.shared_search_request ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_app.shared_search_request ADD COLUMN total_customer_extra_fee numeric(30,2) ;
ALTER TABLE atlas_app.shared_search_request ADD COLUMN total_customer_extra_fee_amount numeric(30,10) ;
ALTER TABLE atlas_app.shared_search_request ADD COLUMN updated_at timestamptz NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.shared_search_request ADD COLUMN valid_till timestamptz NOT NULL;
ALTER TABLE atlas_app.shared_search_request ADD COLUMN vehicle_category character varying(255) NOT NULL;
ALTER TABLE atlas_app.shared_search_request ADD COLUMN waypoints jsonb NOT NULL;
ALTER TABLE atlas_app.shared_search_request ADD PRIMARY KEY ( id);
