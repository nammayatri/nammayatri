ALTER TABLE atlas_app.merchant ADD COLUMN bap_unique_key_id text;
ALTER TABLE atlas_app.merchant ADD COLUMN bap_id text;
ALTER TABLE atlas_app.merchant ADD COLUMN bap_url text;

UPDATE atlas_app.merchant SET bap_unique_key_id='juspay-mobility-bap-1-key';
UPDATE atlas_app.merchant SET bap_id='JUSPAY.CABS.BAP';
UPDATE atlas_app.merchant SET bap_url='http://localhost:8013/cab/v1/';

ALTER TABLE atlas_app.merchant ALTER COLUMN bap_unique_key_id SET NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN bap_id SET NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN bap_url SET NOT NULL;