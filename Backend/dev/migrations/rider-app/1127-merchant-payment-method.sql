CREATE TABLE atlas_app.merchant_payment_method (
    id character(36) NOT NULL PRIMARY KEY,
    merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id),
    payment_type character varying(30) NOT NULL,
    payment_instrument character varying(255) NOT NULL,
    collected_by character varying(30) NOT NULL,
    priority int NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_app.search_request ADD COLUMN available_payment_methods character(36) [] NOT NULL;
ALTER TABLE atlas_app.search_request ADD COLUMN selected_payment_method_id character(36) REFERENCES atlas_app.merchant_payment_method(id);

ALTER TABLE atlas_app.booking ADD COLUMN payment_method_id character(36) REFERENCES atlas_app.merchant_payment_method(id);
ALTER TABLE atlas_app.booking ADD COLUMN payment_url text;

-- local testing data
-- first three should intersect with bpp merchant_payment_method
INSERT INTO atlas_app.merchant_payment_method (id, merchant_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_app.uuid_generate_v4(),
        T1.id,
        'POSTPAID',
        'Card_DefaultCardType',
        'BPP',
        9
    FROM atlas_app.merchant AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.merchant_payment_method (id, merchant_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_app.uuid_generate_v4(),
        T1.id,
        'POSTPAID',
        'Wallet_DefaultWalletType',
        'BAP',
        10
    FROM atlas_app.merchant AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.merchant_payment_method (id, merchant_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_app.uuid_generate_v4(),
        T1.id,
        'POSTPAID',
        'Cash',
        'BPP',
        5
    FROM atlas_app.merchant AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.merchant_payment_method (id, merchant_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_app.uuid_generate_v4(),
        T1.id,
        'POSTPAID',
        'UPI',
        'BAP',
        8
    FROM atlas_app.merchant AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.merchant_payment_method (id, merchant_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_app.uuid_generate_v4(),
        T1.id,
        'POSTPAID',
        'NetBanking',
        'BPP',
        7
    FROM atlas_app.merchant AS T1)
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.merchant_payment_method (id, merchant_id, payment_type, payment_instrument, collected_by, priority)
    (SELECT
        atlas_app.uuid_generate_v4(),
        T1.id,
        'PREPAID',
        'NetBanking',
        'BPP',
        6
    FROM atlas_app.merchant AS T1)
    ON CONFLICT DO NOTHING;
