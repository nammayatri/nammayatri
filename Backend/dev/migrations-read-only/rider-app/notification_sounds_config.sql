CREATE TABLE atlas_app.notification_sounds_config ();

ALTER TABLE atlas_app.notification_sounds_config ADD COLUMN blind_sound text ;
ALTER TABLE atlas_app.notification_sounds_config ADD COLUMN default_sound text ;
ALTER TABLE atlas_app.notification_sounds_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.notification_sounds_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.notification_sounds_config ADD COLUMN notification_type text NOT NULL;
ALTER TABLE atlas_app.notification_sounds_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.notification_sounds_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.notification_sounds_config ADD PRIMARY KEY ( merchant_operating_city_id, notification_type);