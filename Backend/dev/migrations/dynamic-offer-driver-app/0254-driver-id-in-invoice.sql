CREATE TABLE atlas_driver_offer_bpp.notification (
    id CHARACTER(36) PRIMARY KEY NOT NULL,
    short_id CHARACTER VARYING(255) NOT NULL,
    source_amount numeric (30,2) NOT NULL,
    mandate_id CHARACTER VARYING(255) NOT NULL,
    driver_fee_id CHARACTER VARYING(255) NOT NULL,
    txn_date timestamp with time zone NOT NULL,
    juspay_provided_id CHARACTER VARYING(255) NOT NULL,
    provider_name text,
    notification_type text,
    description text not null,
    status text not null,
    date_created timestamp with time zone NOT NULL,
    last_updated timestamp with time zone NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);

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

ALTER TABLE atlas_driver_offer_bpp.driver_fee ALTER COLUMN platform_fee type numeric(30,2) USING platform_fee::numeric(30,2);