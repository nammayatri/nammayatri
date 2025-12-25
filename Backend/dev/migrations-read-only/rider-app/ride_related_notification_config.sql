CREATE TABLE atlas_app.ride_related_notification_config ();

ALTER TABLE atlas_app.ride_related_notification_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ride_related_notification_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ride_related_notification_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ride_related_notification_config ADD COLUMN notification_key text NOT NULL;
ALTER TABLE atlas_app.ride_related_notification_config ADD COLUMN notification_type text NOT NULL;
ALTER TABLE atlas_app.ride_related_notification_config ADD COLUMN only_if_offline boolean NOT NULL;
ALTER TABLE atlas_app.ride_related_notification_config ADD COLUMN time_diff integer NOT NULL;
ALTER TABLE atlas_app.ride_related_notification_config ADD COLUMN time_diff_event text NOT NULL;
ALTER TABLE atlas_app.ride_related_notification_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ride_related_notification_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ride_related_notification_config ADD PRIMARY KEY ( id, merchant_operating_city_id);


------- SQL updates -------

ALTER TABLE atlas_app.ride_related_notification_config ADD COLUMN on_booking_status text NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.ride_related_notification_config ADD COLUMN on_scheduled_booking boolean NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.ride_related_notification_config ADD COLUMN event_time text NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.ride_related_notification_config DROP COLUMN only_if_offline;