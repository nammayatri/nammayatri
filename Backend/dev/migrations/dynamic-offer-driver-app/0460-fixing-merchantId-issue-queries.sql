-- QUERIES FOR PROD AND LOCAL
ALTER TABLE atlas_driver_offer_bpp.issue_category
ADD COLUMN merchant_id character varying (36);

ALTER TABLE atlas_driver_offer_bpp.issue_config
ADD COLUMN merchant_id character varying (36);

ALTER TABLE atlas_driver_offer_bpp.issue_message
ADD COLUMN merchant_id character varying (36);

ALTER TABLE atlas_driver_offer_bpp.issue_option
ADD COLUMN merchant_id character varying (36);

ALTER TABLE atlas_driver_offer_bpp.issue_report
ADD COLUMN merchant_id character varying (36);

ALTER TABLE atlas_driver_offer_bpp.issue_translation
ADD COLUMN merchant_id character varying (36);

ALTER TABLE atlas_driver_offer_bpp.comment
ADD COLUMN merchant_id character varying (36);

-- QUERY FOR MASTER
-- ALTER TABLE atlas_driver_offer_bpp.issue_report
-- ALTER COLUMN merchant_id DROP NOT NULL;

-- ALTER TABLE atlas_driver_offer_bpp.comment
-- ALTER COLUMN merchant_id DROP NOT NULL;

-- QUERY FOR LOCAL, MASTER AND PROD
UPDATE atlas_driver_offer_bpp.issue_category
SET merchant_id = (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant
    WHERE short_id = 'NAMMA_YATRI_PARTNER'
    LIMIT 1
);

UPDATE atlas_driver_offer_bpp.issue_config
SET merchant_id = (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant
    WHERE short_id = 'NAMMA_YATRI_PARTNER'
    LIMIT 1
);

UPDATE atlas_driver_offer_bpp.issue_message
SET merchant_id = (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant
    WHERE short_id = 'NAMMA_YATRI_PARTNER'
    LIMIT 1
);

UPDATE atlas_driver_offer_bpp.issue_option
SET merchant_id = (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant
    WHERE short_id = 'NAMMA_YATRI_PARTNER'
    LIMIT 1
);

UPDATE atlas_driver_offer_bpp.issue_translation
SET merchant_id = (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant
    WHERE short_id = 'NAMMA_YATRI_PARTNER'
    LIMIT 1
);

--QUERIES FOR LOCAL AND PRODUCTION
ALTER TABLE atlas_driver_offer_bpp.issue_category
ALTER COLUMN merchant_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.issue_config
ALTER COLUMN merchant_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.issue_message
ALTER COLUMN merchant_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.issue_option
ALTER COLUMN merchant_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.issue_translation
ALTER COLUMN merchant_id SET NOT NULL;
