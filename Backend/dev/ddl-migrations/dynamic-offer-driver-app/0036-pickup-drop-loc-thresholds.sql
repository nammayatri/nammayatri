-- ONLY FOR LOCAL SYNC
ALTER TABLE atlas_driver_offer_bpp.transporter_config RENAME COLUMN automatic_rc_activation_cut_off TO automatic_r_c_activation_cut_off;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN referral_link_password DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN ride_time_estimated_threshold DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN check_image_extraction_for_dashboard DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN default_popup_delay DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN enable_dashboard_sms DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN onboarding_retry_time_in_hours DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN onboarding_try_limit DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN referral_link_password DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN ride_time_estimated_threshold DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN search_repeat_limit DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN kapture_disposition DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN languages_to_be_translated DROP NOT NULL;