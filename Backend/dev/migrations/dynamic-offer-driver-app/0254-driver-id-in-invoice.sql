UPDATE atlas_driver_offer_bpp.invoice as inv
SET driver_id = df.driver_id
FROM atlas_driver_offer_bpp.driver_fee as df  --- pls validate this query ---
WHERE inv.driver_fee_id = df.id;

update atlas_driver_offer_bpp.driver_information set enabled_at = last_enabled_on;

UPDATE atlas_driver_offer_bpp.driver_information di
SET auto_pay_status = 'PENDING'
FROM atlas_driver_offer_bpp.driver_plan dp
WHERE di.driver_id = dp.driver_id
AND di.auto_pay_status IS NULL;

UPDATE atlas_driver_offer_bpp.driver_information di
SET auto_pay_status = 'MANDATE_FAILED'
FROM atlas_driver_offer_bpp.driver_plan dp
WHERE di.driver_id = dp.driver_id
AND di.auto_pay_status = 'PENDING' and di.created_at <= now() - interval '15 minutes';

ALTER TABLE atlas_driver_offer_bpp.driver_fee ALTER COLUMN platform_fee type numeric(30,2) USING platform_fee::numeric(30,2);