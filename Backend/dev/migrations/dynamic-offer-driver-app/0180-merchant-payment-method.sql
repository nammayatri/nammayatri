-- local testing data
CREATE OR REPLACE FUNCTION atlas_driver_offer_bpp.uuid_generate_v4() RETURNS character (36) AS $uuid_generate_v4$
    BEGIN
        RETURN (uuid_in((md5((random())::text))::cstring));
    END;
$uuid_generate_v4$ LANGUAGE plpgsql;

INSERT INTO atlas_driver_offer_bpp.merchant_payment_method (id, merchant_id, merchant_operating_city_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_driver_offer_bpp.uuid_generate_v4(),
        T1.merchant_id,
        T1.id,
        'POSTPAID',
        'Card_DefaultCardType',
        'BPP',
        9
    FROM atlas_driver_offer_bpp.merchant_operating_city AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.merchant_payment_method (id, merchant_id, merchant_operating_city_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_driver_offer_bpp.uuid_generate_v4(),
        T1.merchant_id,
        T1.id,
        'POSTPAID',
        'Wallet_DefaultWalletType',
        'BAP',
        10
    FROM atlas_driver_offer_bpp.merchant_operating_city AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.merchant_payment_method (id, merchant_id, merchant_operating_city_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_driver_offer_bpp.uuid_generate_v4(),
        T1.merchant_id,
        T1.id,
        'POSTPAID',
        'Cash',
        'BPP',
        5
    FROM atlas_driver_offer_bpp.merchant_operating_city AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.merchant_payment_method (id, merchant_id, merchant_operating_city_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_driver_offer_bpp.uuid_generate_v4(),
        T1.merchant_id,
        T1.id,
        'PREPAID',
        'UPI',
        'BPP',
        8
    FROM atlas_driver_offer_bpp.merchant_operating_city AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.merchant_payment_method (id, merchant_id, merchant_operating_city_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_driver_offer_bpp.uuid_generate_v4(),
        T1.merchant_id,
        T1.id,
        'PREPAID',
        'NetBanking',
        'BAP',
        7
    FROM atlas_driver_offer_bpp.merchant_operating_city AS T1)
    ON CONFLICT DO NOTHING;
