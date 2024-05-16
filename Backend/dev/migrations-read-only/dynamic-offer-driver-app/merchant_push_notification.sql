CREATE TABLE atlas_driver_offer_bpp.merchant_push_notification ();

ALTER TABLE atlas_driver_offer_bpp.merchant_push_notification ADD COLUMN body text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_push_notification ADD COLUMN key text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_push_notification ADD COLUMN language text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_push_notification ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_push_notification ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_push_notification ADD COLUMN title text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_push_notification ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.merchant_push_notification ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.merchant_push_notification ADD PRIMARY KEY ( key);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.merchant_push_notification ADD COLUMN fcm_notification_type text NOT NULL;