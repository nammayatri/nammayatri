CREATE TABLE atlas_app.frfs_cancellation_config ();

ALTER TABLE atlas_app.frfs_cancellation_config ADD COLUMN cancellation_charge_type text NOT NULL;
ALTER TABLE atlas_app.frfs_cancellation_config ADD COLUMN cancellation_charge_value double precision NOT NULL;
ALTER TABLE atlas_app.frfs_cancellation_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_cancellation_config ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_app.frfs_cancellation_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_cancellation_config ADD COLUMN max_minutes_before_departure integer ;
ALTER TABLE atlas_app.frfs_cancellation_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_cancellation_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_cancellation_config ADD COLUMN min_minutes_before_departure integer NOT NULL;
ALTER TABLE atlas_app.frfs_cancellation_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_cancellation_config ADD COLUMN vehicle_category text NOT NULL;
ALTER TABLE atlas_app.frfs_cancellation_config ADD PRIMARY KEY ( id);
