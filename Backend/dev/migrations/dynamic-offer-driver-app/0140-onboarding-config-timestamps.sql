ALTER TABLE atlas_driver_offer_bpp.transporter_config RENAME COLUMN onboarding_retry_timein_hours TO onboarding_retry_time_in_hours;

ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;

-- these columns not used in code
ALTER TABLE atlas_driver_offer_bpp.transporter_config DROP COLUMN max_radius;
ALTER TABLE atlas_driver_offer_bpp.transporter_config DROP COLUMN min_radius;
ALTER TABLE atlas_driver_offer_bpp.transporter_config DROP COLUMN radius_step_size;
