ALTER TABLE atlas_driver_offer_bpp.organization RENAME TO merchant;

DELETE FROM atlas_driver_offer_bpp.merchant
  WHERE type = 'APP' OR type = 'GATEWAY';

ALTER TABLE atlas_driver_offer_bpp.merchant
  DROP COLUMN type,
  DROP COLUMN domain;

ALTER TABLE atlas_driver_offer_bpp.image RENAME COLUMN organization_id TO merchant_id;
ALTER TABLE atlas_driver_offer_bpp.operating_city RENAME COLUMN organization_id TO merchant_id;
ALTER TABLE atlas_driver_offer_bpp.fare_policy RENAME COLUMN organization_id TO merchant_id;
ALTER TABLE atlas_driver_offer_bpp.person RENAME COLUMN organization_id TO merchant_id;
ALTER TABLE atlas_driver_offer_bpp.transporter_config RENAME COLUMN organization_id TO merchant_id;
ALTER TABLE atlas_driver_offer_bpp.vehicle RENAME COLUMN organization_id TO merchant_id;

UPDATE atlas_driver_offer_bpp.booking_cancellation_reason
    SET source = 'ByMerchant'
    WHERE source = 'ByOrganization';

ALTER TABLE atlas_driver_offer_bpp.merchant RENAME COLUMN short_id TO subscriber_id;

ALTER TABLE atlas_driver_offer_bpp.merchant
  ADD COLUMN short_id character varying(255) UNIQUE;

UPDATE atlas_driver_offer_bpp.merchant SET
  short_id = id;

ALTER TABLE atlas_driver_offer_bpp.merchant
  ALTER COLUMN short_id SET NOT NULL;

-- testing data for dev env
UPDATE atlas_driver_offer_bpp.merchant SET
  short_id = 'NAMMA_YATRI_PARTNER'
  WHERE subscriber_id = 'NAMMA_YATRI';

UPDATE atlas_driver_offer_bpp.merchant SET
  short_id = 'OTHER_MERCHANT_2'
  WHERE subscriber_id = 'NAMMA_YATRI_2';
