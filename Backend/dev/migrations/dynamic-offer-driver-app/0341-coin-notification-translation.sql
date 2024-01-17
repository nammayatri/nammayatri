CREATE TABLE atlas_driver_offer_bpp.translations (
    id VARCHAR(36) PRIMARY KEY,
    message_key text NOT NULL,
    language text NOT NULL,
    message text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);

INSERT INTO atlas_driver_offer_bpp.translations (id, message_key, language, message, created_at, updated_at)
SELECT
    id,
    error_type AS message_key,
    language,
    error_message AS message,
    created_at,
    updated_at
FROM atlas_driver_offer_bpp.error_messages_translations;

INSERT INTO atlas_driver_offer_bpp.translations (id, message_Key, language, message, created_at, updated_at)
VALUES
    ('6724c6e5-936c-40d5-85b5-4ca8f1434b6a', 'CoinAdded', 'ENGLISH', 'Coins earned! | You have earned {#coinsValue#} coins. Check them out', NOW(), NOW()),
    ('6724c6e5-936c-40d5-85b5-4ca8f1434b6b', 'CoinAdded', 'HINDI', 'सिक्के कमाएं! | आपने {#coinsValue#} सिक्के कमाएं। उन्हें देखें।', NOW(), NOW()),
    ('6724c6e5-936c-40d5-85b5-4ca8f1434b6c', 'CoinAdded', 'KANNADA', 'ನಾಣ್ಯಗಳನ್ನು ಗಳಿಸಿದ್ದೀರಿ! | ನೀವು {#coinsValue#} ನಾಣ್ಯಗಳನ್ನು ಗಳಿಸಿದ್ದೀರಿ. ಅವುಗಳನ್ನು ಪರಿಶೀಲಿಸಿ.', NOW(), NOW()),
    ('6724c6e5-936c-40d5-85b5-4ca8f1434b6d', 'CoinAdded', 'TAMIL', 'நாணயங்கள் சம்பாதித்துள்ளீர்கள்! | நீங்கள் {#coinsValue#} நாணயங்கள் சம்பாதித்துள்ளீர்கள். அவற்றை சரிபார்க்கவும்', NOW(), NOW()),
    ('6724c6e5-936c-40d5-85b5-4ca8f1434b6e', 'CoinAdded', 'MALAYALAM', 'കോയിനുകൾ സമ്പാദിച്ചു! | നിങ്ങൾ {#coinsValue#} കോയിനുകൾ സമ്പാദിച്ചു. അവയും പരിശോധിക്കുക.', NOW(), NOW()),
    ('6724c6e5-936c-40d5-85b5-4ca8f1434b6f', 'CoinAdded', 'BENGALI', 'মুদ্রা উপার্জন করুন! | আপনি {#coinsValue#} টি মুদ্রা উপার্জন করেছেন। তাদের চেক করুন।', NOW(), NOW()),
    ('6724c6e5-936c-40d5-85b5-4ca8f1434b6g', 'CoinAdded', 'FRENCH', 'Pièces gagnées ! | Vous avez gagné {#coinsValue#} pièces. Vérifiez-les.', NOW(), NOW()),
    ('6724c6e5-936c-40d5-85b5-4ca8f1434b6h', 'CoinAdded', 'TELUGU', 'నాణేలు సంపాదించారు! | మీరు {#coinsValue#} నాణేలు సంపాదించారు. దానిని తనిఖీ చేయండి.', NOW(), NOW());
