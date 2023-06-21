CREATE TABLE atlas_driver_offer_bpp.merchant_payment_method (
    id character(36) NOT NULL PRIMARY KEY,
    merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id),
    payment_type character varying(30) NOT NULL,
    payment_instrument character varying(255) NOT NULL,
    collected_by character varying(30) NOT NULL,
    priority int NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN payment_method_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_payment_method(id);

-- local testing data
CREATE OR REPLACE FUNCTION atlas_driver_offer_bpp.uuid_generate_v4() RETURNS character (36) AS $uuid_generate_v4$
    BEGIN
        RETURN (uuid_in((md5((random())::text))::cstring));
    END;
$uuid_generate_v4$ LANGUAGE plpgsql;

INSERT INTO atlas_driver_offer_bpp.merchant_payment_method (id, merchant_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_driver_offer_bpp.uuid_generate_v4(),
        T1.id,
        'POSTPAID',
        'Card_DefaultCardType',
        'BPP',
        9
    FROM atlas_driver_offer_bpp.merchant AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.merchant_payment_method (id, merchant_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_driver_offer_bpp.uuid_generate_v4(),
        T1.id,
        'POSTPAID',
        'Wallet_DefaultWalletType',
        'BAP',
        10
    FROM atlas_driver_offer_bpp.merchant AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.merchant_payment_method (id, merchant_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_driver_offer_bpp.uuid_generate_v4(),
        T1.id,
        'POSTPAID',
        'Cash',
        'BPP',
        5
    FROM atlas_driver_offer_bpp.merchant AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.merchant_payment_method (id, merchant_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_driver_offer_bpp.uuid_generate_v4(),
        T1.id,
        'PREPAID',
        'UPI',
        'BPP',
        8
    FROM atlas_driver_offer_bpp.merchant AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.merchant_payment_method (id, merchant_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_driver_offer_bpp.uuid_generate_v4(),
        T1.id,
        'PREPAID',
        'NetBanking',
        'BAP',
        7
    FROM atlas_driver_offer_bpp.merchant AS T1)
    ON CONFLICT DO NOTHING;
