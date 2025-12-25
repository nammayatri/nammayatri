CREATE TABLE atlas_driver_offer_bpp.tag_action_notification_config ();

ALTER TABLE atlas_driver_offer_bpp.tag_action_notification_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tag_action_notification_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tag_action_notification_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tag_action_notification_config ADD COLUMN notification_key text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tag_action_notification_config ADD COLUMN notification_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tag_action_notification_config ADD COLUMN notify_at time without time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.tag_action_notification_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.tag_action_notification_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.tag_action_notification_config ADD PRIMARY KEY ( id);
