

ALTER TABLE atlas_driver_offer_bpp.merchant
  DROP COLUMN type,
  DROP COLUMN domain;

ALTER TABLE atlas_driver_offer_bpp.fare_policy RENAME COLUMN organization_id TO merchant_id;
