CREATE TABLE atlas_driver_offer_bpp.reminder_config ();

ALTER TABLE atlas_driver_offer_bpp.reminder_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.reminder_config ADD COLUMN days_threshold integer ;
ALTER TABLE atlas_driver_offer_bpp.reminder_config ADD COLUMN document_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.reminder_config ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.reminder_config ADD COLUMN is_mandatory boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.reminder_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.reminder_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.reminder_config ADD COLUMN reminder_intervals integer[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.reminder_config ADD COLUMN rides_threshold integer ;
ALTER TABLE atlas_driver_offer_bpp.reminder_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.reminder_config ADD PRIMARY KEY ( document_type, merchant_operating_city_id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.reminder_config ADD COLUMN reminder_reschedule_interval_seconds integer ;
ALTER TABLE atlas_driver_offer_bpp.reminder_config ADD COLUMN reminder_on_ride_reschedule_interval_seconds integer ;