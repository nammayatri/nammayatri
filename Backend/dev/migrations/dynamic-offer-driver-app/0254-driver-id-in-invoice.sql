ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN driver_id text;

UPDATE atlas_driver_offer_bpp.invoice as inv
SET inv.driver_id = df.driver_id
FROM atlas_driver_offer_bpp.driver_fee as df  --- pls validate this query ---
WHERE inv.driver_fee_id = df.id;
