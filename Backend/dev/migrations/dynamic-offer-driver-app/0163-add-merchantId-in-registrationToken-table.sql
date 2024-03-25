ALTER TABLE atlas_driver_offer_bpp.registration_token
ADD COLUMN merchant_id TEXT;

UPDATE atlas_driver_offer_bpp.registration_token AS rt
SET merchant_id = (select p.merchant_id from atlas_driver_offer_bpp.person AS p
    where p.id = rt.entity_id);

ALTER TABLE atlas_driver_offer_bpp.registration_token ALTER COLUMN merchant_id SET NOT NULL ;
ALTER TABLE atlas_driver_offer_bpp.registration_token ALTER COLUMN merchant_id SET DEFAULT '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f'