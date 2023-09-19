ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN driver_id text; -- refill it after release

UPDATE atlas_driver_offer_bpp.invoice as inv
SET driver_id = df.driver_id
FROM atlas_driver_offer_bpp.driver_fee as df  --- pls validate this query ---
WHERE inv.driver_fee_id = df.id;

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN is_plan_mandatory boolean not null default false;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN free_trial_days int not null default 0;

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN enabled_at timestamp with time zone;
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