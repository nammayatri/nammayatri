ALTER TABLE atlas_driver_offer_bpp.driver_location ADD COLUMN merchant_id character(36);

WITH PersonAndMerchantIDs AS (
  SELECT p.id, p.merchant_id
  FROM atlas_driver_offer_bpp.person AS p
)
UPDATE atlas_driver_offer_bpp.driver_location
SET merchant_id = PersonAndMerchantIDs.merchant_id
WHERE atlas_driver_offer_bpp.driver_location.driver_id = PersonAndMerchantIDs.id;

ALTER TABLE atlas_driver_offer_bpp.driver_location
ALTER COLUMN merchant_id character(36) NOT NULL DEFAULT "favorit0-0000-0000-0000-00000favorit";