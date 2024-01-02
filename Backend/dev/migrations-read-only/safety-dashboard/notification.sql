CREATE TABLE atlas_safety_dashboard.notification ();

ALTER TABLE atlas_safety_dashboard.notification ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_safety_dashboard.notification ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_safety_dashboard.notification ADD COLUMN merchant_short_id text NOT NULL;
ALTER TABLE atlas_safety_dashboard.notification ADD COLUMN metadata text NOT NULL;
ALTER TABLE atlas_safety_dashboard.notification ADD COLUMN notification_category text NOT NULL;
ALTER TABLE atlas_safety_dashboard.notification ADD COLUMN notification_count integer NOT NULL;
ALTER TABLE atlas_safety_dashboard.notification ADD COLUMN read_status boolean NOT NULL;
ALTER TABLE atlas_safety_dashboard.notification ADD COLUMN receiver_id text NOT NULL;
ALTER TABLE atlas_safety_dashboard.notification ADD COLUMN sender_id text NOT NULL;
ALTER TABLE atlas_safety_dashboard.notification ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_safety_dashboard.notification ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_safety_dashboard.notification ADD PRIMARY KEY ( id);