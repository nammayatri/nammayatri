ALTER TABLE atlas_app.registration_token
ADD COLUMN merchant_id TEXT;

UPDATE atlas_app.registration_token AS rt
SET merchant_id = (select p.merchant_id from atlas_app.person AS p
    where p.id = rt.entity_id);

ALTER TABLE atlas_app.registration_token ALTER COLUMN merchant_id SET NOT NULL ;
ALTER TABLE atlas_app.registration_token ALTER COLUMN merchant_id SET DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51'