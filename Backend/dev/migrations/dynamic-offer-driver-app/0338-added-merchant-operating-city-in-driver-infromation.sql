ALTER TABLE atlas_driver_offer_bpp.driver_information
ADD COLUMN merchant_operating_city_id character varying(36);

-- Backfilling merchant_operating_city_id in driver_information table
UPDATE atlas_driver_offer_bpp.driver_information AS di
SET merchant_operating_city_id = p.merchant_operating_city_id
FROM atlas_driver_offer_bpp.person AS p
WHERE di.driver_id = p.id and p.merchant_operating_city_id is not null;
