ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_fee_mandate_notification_batch_size int DEFAULT 20 NOT NULL; -- Notification scheduled at 9am
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_fee_mandate_execution_batch_size int DEFAULT 20 NOT NULL; -- Execution scheduled at next day 2pm
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN mandate_notification_reschedule_interval bigint DEFAULT 60 NOT NULL; -- Invoice generation from 1st September
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN mandate_execution_reschedule_interval bigint DEFAULT 60 NOT NULL;
-- ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN mandate_validity bigint DEFAULT 60  NOT NULL;