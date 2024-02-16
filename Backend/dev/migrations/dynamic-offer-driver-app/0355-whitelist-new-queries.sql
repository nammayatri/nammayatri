ALTER TABLE atlas_driver_offer_bpp.white_list_org
ADD COLUMN domain character varying(255) default NULL;

INSERT INTO atlas_driver_offer_bpp.white_list_org (id, subscriber_id, type, domain)
SELECT
    (md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID) AS id,
    subscriber_id,
    'BAP' AS type,
    'MOBILITY' AS domain
FROM atlas_driver_offer_bpp.white_list_org
WHERE type = 'APP';

UPDATE atlas_driver_offer_bpp.white_list_org
SET domain = 'MOBILITY_OLD'
WHERE type = 'APP';

ALTER TABLE atlas_driver_offer_bpp.white_list_org
ALTER COLUMN domain SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.black_list_org
ADD COLUMN domain character varying(255) NOT NULL;