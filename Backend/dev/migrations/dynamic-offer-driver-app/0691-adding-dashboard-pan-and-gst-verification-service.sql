ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD  COLUMN
dashboard_pan_verification_service character varying(30) ;

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config  ADD COLUMN
dashboard_gst_verification_service  character varying(30) ;

-- please run this also , after running above query
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config --
SET
    dashboard_pan_verification_service = 'Idfy',
    dashboard_gst_verification_service = 'Idfy';
