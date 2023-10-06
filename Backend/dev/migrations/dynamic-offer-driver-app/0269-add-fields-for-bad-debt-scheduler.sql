
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN bad_debt_batch_size int DEFAULT 1000;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN bad_debt_reschedule_time bigint DEFAULT 60;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN bad_debt_sechuler_time bigint DEFAULT 345600; ---- to be configured
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN bad_debt_time_threshold bigint DEFAULT 345600; ---- to be configured

 ----------  driver fee --------------

ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN bad_debt_declaration_date timestamp with time zone;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN bad_debt_recovery_date timestamp with time zone;
