ALTER TABLE atlas_transporter.organization RENAME TO merchant;

DELETE FROM atlas_transporter.fare_policy as T1
  USING atlas_transporter.merchant as T2
  WHERE (T1.organization_id = T2.id) AND (T2.type = 'APP' OR T2.type = 'GATEWAY');

DELETE FROM atlas_transporter.one_way_fare_policy_per_extra_km_rate as T1
  USING atlas_transporter.merchant as T2
  WHERE (T1.organization_id = T2.id) AND (T2.type = 'APP' OR T2.type = 'GATEWAY');

DELETE FROM atlas_transporter.merchant
  WHERE type = 'APP' OR type = 'GATEWAY';

ALTER TABLE atlas_transporter.merchant
  DROP COLUMN type,
  DROP COLUMN domain;

ALTER TABLE atlas_transporter.one_way_fare_policy_per_extra_km_rate RENAME COLUMN organization_id TO merchant_id;
ALTER TABLE atlas_transporter.fare_policy_discount RENAME COLUMN organization_id TO merchant_id;
ALTER TABLE atlas_transporter.fare_product RENAME COLUMN organization_id TO merchant_id;
ALTER TABLE atlas_transporter.fare_policy RENAME COLUMN organization_id TO merchant_id;
ALTER TABLE atlas_transporter.rental_fare_policy RENAME COLUMN organization_id TO merchant_id;
ALTER TABLE atlas_transporter.discount_transaction RENAME COLUMN organization_id TO merchant_id;
ALTER TABLE atlas_transporter.person RENAME COLUMN organization_id TO merchant_id;
ALTER TABLE atlas_transporter.ride_request RENAME COLUMN short_org_id TO subscriber_id;
ALTER TABLE atlas_transporter.vehicle RENAME COLUMN organization_id TO merchant_id;

UPDATE atlas_transporter.booking_cancellation_reason
    SET source = 'ByMerchant'
    WHERE source = 'ByOrganization';

ALTER TABLE atlas_transporter.merchant RENAME COLUMN short_id TO subscriber_id;

ALTER TABLE atlas_transporter.merchant
  ADD COLUMN short_id character varying(255) UNIQUE;

UPDATE atlas_transporter.merchant SET
  short_id = id;

ALTER TABLE atlas_transporter.merchant
  ALTER COLUMN short_id SET NOT NULL;

-- testing data for different envs
UPDATE atlas_transporter.merchant SET
  short_id = 'YATRI_PARTNER'
  WHERE subscriber_id
  IN (
      'YATRI',
      'api.sandbox.beckn.juspay.in/dev/bpp/cab/v1/565db72a-04d4-4211-90ae-c956461397b2',
      'api.beckn.juspay.in/bpp/cab/v1/3c5fa6ae-2e90-4bb9-818e-7bb109b4cca3',
      'api.sandbox.beckn.juspay.in/bpp/cab/v1/3041599b-2fcf-45e1-bfd5-115db5cd1353'
    );

UPDATE atlas_transporter.merchant SET
  short_id = 'OTHER_MERCHANT'
  WHERE subscriber_id = 'YATRI_2';
