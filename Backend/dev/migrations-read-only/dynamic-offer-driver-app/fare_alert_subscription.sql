CREATE TABLE atlas_driver_offer_bpp.fare_alert_subscription ();

ALTER TABLE atlas_driver_offer_bpp.fare_alert_subscription ADD COLUMN alert_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_alert_subscription ADD COLUMN email text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_alert_subscription ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_alert_subscription ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_alert_subscription ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_alert_subscription ADD COLUMN subscribed_by text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_alert_subscription ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fare_alert_subscription ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fare_alert_subscription ADD PRIMARY KEY ( id);
