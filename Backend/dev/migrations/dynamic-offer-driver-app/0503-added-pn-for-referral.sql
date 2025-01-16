INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'REFERRAL_FLOW',
    'REFERRAL_FLOW',
    moc.merchant_id,
    moc.id,
    '🎉 Great news! A rider just joined Namma Yatri using your referral code! 🚀',
    'Keep sharing your code and earn more rewards. Let''s grow together! ✨',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'REFERRAL_FLOW',
    'REFERRAL_FLOW',
    moc.merchant_id,
    moc.id,
    '🎉 ಉತ್ತಮ ಸುದ್ದಿ! ನಿಮ್ಮ ರೆಫರಲ್ ಕೋಡ್ ಅನ್ನು ಬಳಸಿಕೊಂಡು ಸವಾರರೊಬ್ಬರು ನಮ್ಮ ಯಾತ್ರಿಗೆ ಸೇರಿದ್ದಾರೆ! 🚀',
    'ನಿಮ್ಮ ಕೋಡ್ ಹಂಚಿಕೊಳ್ಳುವುದನ್ನು ಮುಂದುವರಿಸಿ ಮತ್ತು ಹೆಚ್ಚಿನ ಬಹುಮಾನಗಳನ್ನು ಗಳಿಸಿ. ಒಟ್ಟಿಗೆ ಬೆಳೆಯೋಣ! ✨',
    'KANNADA',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'REFERRAL_FLOW',
    'REFERRAL_FLOW',
    moc.merchant_id,
    moc.id,
    '🎉 గుడ్ న్యూస్! ఓ రైడర్ మీ రిఫరల్ కోడ్ ఉపయోగించి ఇప్పుడే నమ్మ యాత్రిలో చేరాడు! 🚀',
    'మీ కోడ్‌ను షేర్ చేస్తూ మరిన్ని బహుమతులు పొందండి. మనం కలిసి ఎదగుదాం! ✨',
    'TELUGU',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'REFERRAL_FLOW',
    'REFERRAL_FLOW',
    moc.merchant_id,
    moc.id,
    '🎉 बहुत अच्छी खबर! आपके रेफरल कोड का उपयोग करके एक राइडर नम्मा यात्री में शामिल हो गया है! 🚀',
    'अपना कोड शेयर करते रहें और ज़्यादा पुरस्कार जीतें। ✨',
    'HINDI',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'REFERRAL_FLOW',
    'REFERRAL_FLOW',
    moc.merchant_id,
    moc.id,
    '🎉 நல்ல செய்தி! உங்கள் பரிந்துரைக் குறியீட்டைப் பயன்படுத்தி ஒரு ரைடர் நம்ம யாத்ரியில் சேர்ந்துள்ளார்! 🚀',
    'உங்கள் குறியீட்டைத் தொடர்ந்து பகிர்ந்து, அதிக வெகுமதிகளைப் பெறுங்கள். ஒன்றாக வளர்வோம்! ✨',
    'TAMIL',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;