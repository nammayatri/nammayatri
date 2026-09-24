-- NOTE: dont need to run these queries


ALTER TABLE atlas_driver_offer_bpp.merchant
  DROP COLUMN type,
  DROP COLUMN domain;
DO $$ BEGIN

ALTER TABLE atlas_driver_offer_bpp.fare_policy RENAME COLUMN organization_id TO merchant_id;
EXCEPTION WHEN OTHERS THEN NULL;
END $$;
-- NOTE: dont need to run these queries
