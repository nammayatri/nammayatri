ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_fee_retry_threshold_config int NOT NULL DEFAULT 3; -- this gets added to endTime of window
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN update_notification_status_batch_size int NOT NULL DEFAULT 20;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN update_order_status_batch_size int NOT NULL DEFAULT 20;
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN last_status_checked_at timestamp with time zone;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN last_status_checked_at timestamp with time zone;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN scheduler_try_count int NOT NULL DEFAULT 1;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN order_and_notification_status_check_time bigint DEFAULT 63000;