UPDATE atlas_app.merchant as T1 SET bap_unique_key_id='juspay-mobility-bap-1-key', bap_id=concat('localhost/beckn/cab/v1/', T1.id);
ALTER TABLE atlas_app.merchant ALTER COLUMN bap_unique_key_id SET NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN bap_id SET NOT NULL;