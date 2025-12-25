-- local testing data
-- first three should intersect with bpp merchant_payment_method
INSERT INTO atlas_app.merchant_payment_method (id, merchant_id, merchant_operating_city_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_app.uuid_generate_v4(),
        T1.merchant_id,
        T1.id,
        'POSTPAID',
        'Card_DefaultCardType',
        'BPP',
        9
    FROM atlas_app.merchant_operating_city AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.merchant_payment_method (id, merchant_id, merchant_operating_city_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_app.uuid_generate_v4(),
        T1.merchant_id,
        T1.id,
        'POSTPAID',
        'Wallet_DefaultWalletType',
        'BAP',
        10
    FROM atlas_app.merchant_operating_city AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.merchant_payment_method (id, merchant_id, merchant_operating_city_id ,payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_app.uuid_generate_v4(),
        T1.merchant_id,
        T1.id,
        'POSTPAID',
        'Cash',
        'BPP',
        5
    FROM atlas_app.merchant_operating_city AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.merchant_payment_method (id, merchant_id, merchant_operating_city_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_app.uuid_generate_v4(),
        T1.merchant_id,
        T1.id,
        'POSTPAID',
        'UPI',
        'BAP',
        8
    FROM atlas_app.merchant_operating_city AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.merchant_payment_method (id, merchant_id, merchant_operating_city_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_app.uuid_generate_v4(),
        T1.merchant_id,
        T1.id,
        'POSTPAID',
        'NetBanking',
        'BPP',
        7
    FROM atlas_app.merchant_operating_city AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.merchant_payment_method (id, merchant_id, merchant_operating_city_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_app.uuid_generate_v4(),
        T1.merchant_id,
        T1.id,
        'PREPAID',
        'NetBanking',
        'BPP',
        6
    FROM atlas_app.merchant_operating_city AS T1)
    ON CONFLICT DO NOTHING;
