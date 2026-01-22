CREATE TABLE atlas_dashboard.transaction ();

ALTER TABLE atlas_dashboard.transaction ADD COLUMN common_driver_id text ;
ALTER TABLE atlas_dashboard.transaction ADD COLUMN common_ride_id text ;
ALTER TABLE atlas_dashboard.transaction ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_dashboard.transaction ADD COLUMN endpoint text NOT NULL;
ALTER TABLE atlas_dashboard.transaction ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_dashboard.transaction ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_dashboard.transaction ADD COLUMN request text ;
ALTER TABLE atlas_dashboard.transaction ADD COLUMN requestor_id character varying(36) ;
ALTER TABLE atlas_dashboard.transaction ADD COLUMN response text ;
ALTER TABLE atlas_dashboard.transaction ADD COLUMN response_error text ;
ALTER TABLE atlas_dashboard.transaction ADD COLUMN server_name text ;
ALTER TABLE atlas_dashboard.transaction ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_dashboard.transaction ADD PRIMARY KEY ( id);
