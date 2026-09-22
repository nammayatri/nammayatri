UPDATE atlas_driver_offer_bpp.bap_metadata b
SET enable_ondc_scheduled_ride_support = true
FROM atlas_driver_offer_bpp.transporter_config tc
WHERE tc.merchant_operating_city_id = b.merchant_operating_city_id
  AND tc.enable_ondc_scheduled_ride_support = true;

ALTER TABLE atlas_driver_offer_bpp.bap_metadata DROP CONSTRAINT bap_metadata_pkey;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD PRIMARY KEY (domain, id, merchant_id, merchant_operating_city_id);

ALTER TABLE atlas_driver_offer_bpp.transporter_config DROP COLUMN enable_ondc_scheduled_ride_support;
