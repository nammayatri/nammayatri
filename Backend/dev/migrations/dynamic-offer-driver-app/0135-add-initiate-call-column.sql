ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config
    ADD COLUMN initiate_call character varying(30);
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
    SET initiate_call = 'Exotel';
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ALTER COLUMN initiate_call SET NOT NULL;