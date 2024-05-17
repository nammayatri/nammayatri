UPDATE atlas_driver_offer_bpp.registration_token AS rt
SET merchant_id = (select p.merchant_id from atlas_driver_offer_bpp.person AS p
    where p.id = rt.entity_id);

ALTER TABLE atlas_driver_offer_bpp.registration_token ALTER COLUMN merchant_id SET NOT NULL ;
ALTER TABLE atlas_driver_offer_bpp.registration_token ALTER COLUMN merchant_id SET DEFAULT 'favorit0-0000-0000-0000-00000favorit'