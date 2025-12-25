CREATE TABLE atlas_app.ny_regular_instance_log ();

ALTER TABLE atlas_app.ny_regular_instance_log ADD COLUMN automation_status text NOT NULL;
ALTER TABLE atlas_app.ny_regular_instance_log ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ny_regular_instance_log ADD COLUMN instance_transaction_id text NOT NULL;
ALTER TABLE atlas_app.ny_regular_instance_log ADD COLUMN ny_regular_subscription_id character(36) NOT NULL;
ALTER TABLE atlas_app.ny_regular_instance_log ADD COLUMN scheduled_pickup_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.ny_regular_instance_log ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ny_regular_instance_log ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.ny_regular_instance_log ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.ny_regular_instance_log ADD PRIMARY KEY ( instance_transaction_id);
