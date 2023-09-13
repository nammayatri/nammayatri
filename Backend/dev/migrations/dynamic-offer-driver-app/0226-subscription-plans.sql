ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_auto_pay_notification_time bigint DEFAULT 32400; -- Notification scheduled at 9am
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_auto_pay_execution_time bigint DEFAULT 104400; -- Execution scheduled at next day 2pm
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN subscription_start_time timestamp with time zone DEFAULT TIMESTAMP '2023-08-31 00:00:00' NOT NULL; -- Invoice generation from 1st September
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN mandate_validity Int DEFAULT 5  NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN fee_type text NOT NULL DEFAULT 'RECURRING_INVOICE';
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN merchant_id character (36) NOT NULL DEFAULT 'favorit0-0000-0000-0000-00000favorit'; -- Default should be yatri saathi merchant_id

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.driver_plan
    (   driver_id character(36) NOT NULL,
        plan_id character(36) NOT NULL,
        plan_type text NOT NULL,
        mandate_id text,
        created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
        updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
    );
ALTER TABLE atlas_driver_offer_bpp.driver_plan OWNER TO atlas_driver_offer_bpp_user;
ALTER TABLE atlas_driver_offer_bpp.driver_plan ADD COLUMN mandate_setup_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP;

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.mandate
    (   id text NOT NULL PRIMARY KEY,
        max_amount integer NOT NULL,
        status text NOT NULL,
        payer_vpa text,
        start_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
        end_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
        created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
        updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
    );
ALTER TABLE atlas_driver_offer_bpp.mandate OWNER TO atlas_driver_offer_bpp_user;
ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN payer_app Text;
ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN payer_app_name Text;


CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.plan
    (   id character(36) NOT NULL,
        merchant_id character(36) NOT NULL,
        payment_mode text NOT NULL,
        plan_type text NOT NULL,
        name text NOT NULL,
        description text NOT NULL,
        max_amount integer NOT NULL,
        registration_amount integer NOT NULL,
        plan_base_amount text NOT NULL,
        is_offer_applicable boolean NOT NULL,
        max_credit_limit integer NOT NULL,
        free_ride_count integer NOT NULL,
        frequency text NOT NULL,
        PRIMARY KEY(id, payment_mode)
    );
ALTER TABLE atlas_driver_offer_bpp.plan OWNER TO atlas_driver_offer_bpp_user;
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.invoice
    (   id character(36) NOT NULL,
        invoice_short_id text NOT NULL,
        driver_fee_id Text,
        invoice_status Text,
        created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
        updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
        PRIMARY KEY(id, driver_fee_id)
    );
ALTER TABLE atlas_driver_offer_bpp.invoice OWNER TO atlas_driver_offer_bpp_user;
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN max_mandate_amount integer;

-- This is to backfill existing 1 to 1 mapped yatri saathi entries of driver fees to invoice table
INSERT INTO atlas_driver_offer_bpp.invoice (id, invoice_short_id, driver_fee_id) SELECT PO.id, PO.short_id, PO.id FROM atlas_driver_offer_bpp.payment_order AS PO INNER JOIN atlas_driver_offer_bpp.driver_fee AS DF ON DF.short_id = PO.short_id;

-- run after release
INSERT INTO atlas_driver_offer_bpp.invoice (id, invoice_short_id, driver_fee_id) SELECT PO.id, PO.short_id, PO.id FROM atlas_driver_offer_bpp.payment_order AS PO INNER JOIN atlas_driver_offer_bpp.driver_fee AS DF ON DF.short_id = PO.short_id on conflict do nothing;

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN auto_pay_status text;

INSERT INTO atlas_driver_offer_bpp.plan (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type) VALUES
    ('a35ffc7c-de0d-4dcc-83a8-e36a5a29cc1d', 'favorit0-0000-0000-0000-00000favorit', 'MANUAL', 'DAILY', 'DAILY_25.0', 'DAILY UNLIMITED' , 'Enjoy UNLIMITED rides, every day!', 25, 1, true, 100, 1, 'SUBSCRIPTION'), -- Keep same in prod and master for daily unlimited plan, frontend has hardcoded
    ('a35ffc7c-de0d-4dcc-83a8-e36a5a29cc1d', 'favorit0-0000-0000-0000-00000favorit', 'AUTOPAY', 'DAILY', 'DAILY_25.0', 'DAILY UNLIMITED' , 'Enjoy UNLIMITED rides, every day!', 25, 1, true, 100, 1, 'SUBSCRIPTION'),
    ('18911beb-28ba-456d-8cca-4d019461d2b0', 'favorit0-0000-0000-0000-00000favorit', 'MANUAL', 'DAILY', 'PERRIDE_3.5', 'DAILY PER RIDE' , 'Up to a maximum of ₹35 per day', 35, 1, false, 100, 0, 'DEFAULT'),
    ('18911beb-28ba-456d-8cca-4d019461d2b0', 'favorit0-0000-0000-0000-00000favorit', 'AUTOPAY', 'DAILY', 'PERRIDE_3.5', 'DAILY PER RIDE' , 'Up to a maximum of ₹35 per day', 35, 1, true, 100, 0, 'SUBSCRIPTION');

ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN create_mandate text;
ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN mandate_max_amount integer;
ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN mandate_start_date timestamp with time zone;
ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN mandate_end_date timestamp with time zone;
ALTER TABLE atlas_driver_offer_bpp.payment_transaction ADD COLUMN mandate_id text;
ALTER TABLE atlas_driver_offer_bpp.payment_transaction ADD COLUMN mandate_max_amount integer;
ALTER TABLE atlas_driver_offer_bpp.payment_transaction ADD COLUMN mandate_frequency text;
ALTER TABLE atlas_driver_offer_bpp.payment_transaction ADD COLUMN mandate_status text;
ALTER TABLE atlas_driver_offer_bpp.payment_transaction ADD COLUMN mandate_start_date timestamp with time zone;
ALTER TABLE atlas_driver_offer_bpp.payment_transaction ADD COLUMN mandate_end_date timestamp with time zone;

ALTER TABLE atlas_driver_offer_bpp.driver_fee ALTER COLUMN short_id DROP NOT NULL;

-------------------------------------------------------------------------------
---------------------------------- DROP ---------------------------------------
-------------------------------------------------------------------------------
ALTER TABLE atlas_driver_offer_bpp.driver_fee DROP COLUMN IF EXISTS short_id;

UPDATE atlas_driver_offer_bpp.driver_plan as dp
SET mandate_setup_date = (SELECT updated_at FROM atlas_driver_offer_bpp.driver_fee as df WHERE df.driver_id = dp.driver_id AND df.fee_type = 'MANDATE_REGISTRATION' AND df.status = 'CLEARED' order by updated_at desc limit 1)