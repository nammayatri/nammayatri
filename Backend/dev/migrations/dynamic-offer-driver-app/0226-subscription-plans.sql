CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.driver_plan
    (   driver_id character(36) NOT NULL,
        plan_id character(36) NOT NULL,
        plan_type text NOT NULL,
        mandate_id text,
        created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
        updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
    );
ALTER TABLE atlas_driver_offer_bpp.driver_plan OWNER TO atlas_driver_offer_bpp_user;

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

INSERT INTO atlas_driver_offer_bpp.plan (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage) VALUES
    ('a35ffc7c-de0d-4dcc-83a8-e36a5a29cc1c', 'favorit0-0000-0000-0000-00000favorit', 'MANUAL', 'DAILY', 'DAILY_25.0', 'DAILY UNLIMITED' , 'Enjoy UNLIMITED rides, every day!', 25, 1, true, 100, 1, 'SUBSCRIPTION', 0.09, 35.0, 'mOpCityId', 0.09), -- Keep same in prod and master for daily unlimited plan, frontend has hardcoded
    ('a35ffc7c-de0d-4dcc-83a8-e36a5a29cc1d', 'favorit0-0000-0000-0000-00000favorit', 'AUTOPAY', 'DAILY', 'DAILY_25.0', 'DAILY UNLIMITED' , 'Enjoy UNLIMITED rides, every day!', 25, 1, true, 100, 1, 'SUBSCRIPTION', 0.09, 25.0, 'mOpCityId', 0.09),
    ('18911beb-28ba-456d-8cca-4d019461d2b1', 'favorit0-0000-0000-0000-00000favorit', 'MANUAL', 'DAILY', 'PERRIDE_3.5', 'DAILY PER RIDE' , 'Up to a maximum of ₹35 per day', 35, 1, false, 100, 0, 'DEFAULT', 0.09, 35.0, 'mOpCityId', 0.09),
    ('18911beb-28ba-456d-8cca-4d019461d2b0', 'favorit0-0000-0000-0000-00000favorit', 'AUTOPAY', 'DAILY', 'PERRIDE_3.5', 'DAILY PER RIDE' , 'Up to a maximum of ₹35 per day', 35, 1, true, 100, 0, 'SUBSCRIPTION', 0.09, 35.0, 'mOpCityId', 0.09);

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


-------------------------------------------------------------------------------
---------------------------------- DROP ---------------------------------------
-------------------------------------------------------------------------------
ALTER TABLE atlas_driver_offer_bpp.driver_fee DROP COLUMN IF EXISTS short_id;
