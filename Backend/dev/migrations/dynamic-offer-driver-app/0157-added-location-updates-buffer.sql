ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN future_driver_location_buffer_in_sec bigint DEFAULT 90 NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN merchant_id character(36);


UPDATE atlas_driver_offer_bpp.driver_information AS di SET merchant_id = p.merchant_id
  FROM atlas_driver_offer_bpp.person AS p
    WHERE ( p.role = 'DRIVER' AND p.id = di.driver_id);

ALTER TABLE atlas_driver_offer_bpp.driver_information
ALTER COLUMN merchant_id SET NOT NULL;