CREATE INDEX idx_rider_details_ride_id ON atlas_driver_offer_bpp.rider_details USING btree (first_ride_id);

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYOUT_VPA_REMINDER',
    'PAYOUT_VPA_ALERT',
    moc.merchant_id,
    moc.id,
    '₹{#rewardAmount#} ರ ರೆಫರಲ್ ಬೋನಸ್ ಸ್ವೀಕರಿಸಲು UPI ಐಡಿ ಸೇರಿಸಿ',
    'UPI ಐಡಿ ಇಲ್ಲದಿರುವ ಕಾರಣ ರೆಫರಲ್ ಬೋನಸ್ ವಹಿವಾಟು ಬಾಕಿ ಉಳಿದಿದೆ',
    'KANNADA',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYOUT_VPA_REMINDER',
    'PAYOUT_VPA_ALERT',
    moc.merchant_id,
    moc.id,
    'பரிந்துரை போனஸ் ₹{#rewardAmount#} பெற UPI ஐடியைச் சேர்க்கவும்',
    'UPI ஐடி விடுபட்டதால் பரிந்துரை போனஸ் பரிவர்த்தனை நிலுவையில் உள்ளது.',
    'TAMIL',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYOUT_VPA_REMINDER',
    'PAYOUT_VPA_ALERT',
    moc.merchant_id,
    moc.id,
    '₹{#rewardAmount#} రిఫరల్ బోనస్ పొందడానికి మీ UPI ID ని జోడించండి',
    'UPI ID లేని కారణంగా రిఫరల్ బోనస్ లావాదేవీ పెండింగ్‌లో ఉంది',
    'TELUGU',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYOUT_VPA_REMINDER',
    'PAYOUT_VPA_ALERT',
    moc.merchant_id,
    moc.id,
    '₹{#rewardAmount#} का रेफरल बोनस प्राप्त करने के लिए UPI ID जोड़ें',
    'UPI ID ना होने के कारण रेफरल बोनस लेनदेन पूरा नहीं हुआ है',
    'HINDI',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

---------------------------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYOUT_REWARD',
    'PAYOUT_REFERRAL_REWARD',
    moc.merchant_id,
    moc.id,
    '₹{#rewardAmount#} ರೆಫರಲ್ ಬೋನಸ್ ಗಳಿಸಲಾಗಿದೆ',
    'ರೆಫರಲ್ ಗ್ರಾಹಕರು ತಮ್ಮ ಮೊದಲ ಸವಾರಿಯನ್ನು ಪೂರ್ಣಗೊಳಿಸಿದ್ದಾರೆ. ಹೆಚ್ಚು ಹಣ ಗಳಿಸಲು ರೆಫರ್ ಮಾಡಿ',
    'KANNADA',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYOUT_REWARD',
    'PAYOUT_REFERRAL_REWARD',
    moc.merchant_id,
    moc.id,
    '₹{#rewardAmount#} பரிந்துரை போனஸ் சம்பாதிக்கப்பட்டது',
    'பரிந்துரைக்கப்பட்ட வாடிக்கையாளர் தங்கள் முதல் பயணத்தை முடித்தார். தொடர்ந்து குறிப்பிடவும் மேலும் சம்பாதிக்கவும்.',
    'TAMIL',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYOUT_REWARD',
    'PAYOUT_REFERRAL_REWARD',
    moc.merchant_id,
    moc.id,
    '₹{#rewardAmount#} రిఫరల్ బోనస్ సంపాదించారు',
    'మీరు రిఫర్ చేసిన కస్టమర్ వారి మొదటి రైడ్ పూర్తి చేశారు. మరింత సంపాదించడానికి మరిన్ని రిఫరల్స్ చేయండి',
    'TELUGU',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYOUT_REWARD',
    'PAYOUT_REFERRAL_REWARD',
    moc.merchant_id,
    moc.id,
    '₹{#rewardAmount#} रेफरल बोनस प्राप्त हुआ',
    'रेफ़र किए गए ग्राहक ने अपनी पहली राइड पूरी कर ली है।अधिक कमाने के लिए रेफर करते रहे.',
    'HINDI',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;