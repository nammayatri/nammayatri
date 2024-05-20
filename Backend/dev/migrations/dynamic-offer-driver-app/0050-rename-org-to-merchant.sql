DELETE FROM atlas_driver_offer_bpp.merchant
  WHERE type = 'APP' OR type = 'GATEWAY';

ALTER TABLE atlas_driver_offer_bpp.merchant
  DROP COLUMN type,
  DROP COLUMN domain;

ALTER TABLE atlas_driver_offer_bpp.fare_policy RENAME COLUMN organization_id TO merchant_id;

UPDATE atlas_driver_offer_bpp.booking_cancellation_reason
    SET source = 'ByMerchant'
    WHERE source = 'ByOrganization';


-- testing data for dev env
UPDATE atlas_driver_offer_bpp.merchant SET
  short_id = 'NAMMA_YATRI_PARTNER'
  WHERE subscriber_id = 'NAMMA_YATRI';

UPDATE atlas_driver_offer_bpp.merchant SET
  short_id = 'OTHER_MERCHANT_2'
  WHERE subscriber_id = 'NAMMA_YATRI_2';
