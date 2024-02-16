ALTER TABLE atlas_app.white_list_org
ADD COLUMN domain character varying(255) default NULL;

INSERT INTO atlas_app.white_list_org (id, subscriber_id, type, domain)
SELECT
    (md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID) AS id,
    subscriber_id,
    'BPP' AS type,
    'MOBILITY' AS domain
FROM atlas_app.white_list_org
WHERE type = 'PROVIDER';

UPDATE atlas_driver_offer_bpp.white_list_org
SET domain = 'MOBILITY_OLD'
WHERE type = 'PROVIDER';

ALTER TABLE atlas_app.white_list_org
ALTER COLUMN domain SET NOT NULL;

ALTER TABLE atlas_app.black_list_org
ADD COLUMN domain character varying(255) NOT NULL;
