INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FCM_CHAT_MESSAGE',
    'FCM_CHAT_MESSAGE',
    moc.merchant_id,
    moc.id,
    'Missed Call: Quick Action Required',
    'Your driver tried to contact you but couldn''t get through. Please call them back to coordinate your pickup.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FCM_CHAT_MESSAGE',
    'FCM_CHAT_MESSAGE',
    moc.merchant_id,
    moc.id,
    'ಕಳೆದುಹೋಗಿದ ಕರೆ: ತ್ವರಿತ ಕ್ರಮ ಅಗತ್ಯ',
    'ನಿಮ್ಮ ಚಾಲಕ ನಿಮಗೆ ಕರೆಮಾಡಲು ಪ್ರಯತ್ನಿಸಿದರು ಆದರೆ ಸಂಪರ್ಕ ಆಗಲಿಲ್ಲ. ದಯವಿಟ್ಟು ಅವರಿಗೆ ಮರುಕೊಳ್ಳಲು ಕರೆ ಮಾಡಿ ಮತ್ತು ನಿಮ್ಮ ಪಿಕಪ್ ಅನ್ನು ಹೊಂದಿಸಿ.',
    'KANNADA',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FCM_CHAT_MESSAGE',
    'FCM_CHAT_MESSAGE',
    moc.merchant_id,
    moc.id,
    'നഷ്ടമായ ഫോൺകാൾ: വേഗത്തിലുള്ള നടപടി ആവശ്യമാണ്',
    'നിങ്ങളുടെ ഡ്രൈവർ നിങ്ങളെ വിളിക്കാൻ ശ്രമിച്ചെങ്കിലും വിളി ലഭിച്ചില്ല. ദയവായി തിരിച്ച് വിളിച്ച് നിങ്ങളുടെ പിക്കപ്പ് ഏകീകരിക്കുക.',
    'MALAYALAM',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FCM_CHAT_MESSAGE',
    'FCM_CHAT_MESSAGE',
    moc.merchant_id,
    moc.id,
    'మిస్ కాల్: తక్షణ చర్య అవసరం',
    'మీ డ్రైవర్ మీకు ఫోన్ చేయడానికి ప్రయత్నించాడు కానీ ఫోన్ కలవలేదు. దయచేసి తిరిగి కాల్ చేసి, మీ పికప్‌ను సెట్ చేసుకోండి.',
    'TELUGU',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FCM_CHAT_MESSAGE',
    'FCM_CHAT_MESSAGE',
    moc.merchant_id,
    moc.id,
    'मिस्ड कॉल: तुरंत कार्रवाई आवश्यक',
    'आपके ड्राइवर ने आपको कॉल करने की कोशिश की लेकिन कॉल नहीं लग पाई। कृपया वापस कॉल करें और अपने पिकअप को तय करें।',
    'HINDI',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FCM_CHAT_MESSAGE',
    'FCM_CHAT_MESSAGE',
    moc.merchant_id,
    moc.id,
    'মিসড কল: দ্রুত পদক্ষেপ প্রয়োজন',
    'আপনার ড্রাইভার আপনাকে কল করার চেষ্টা করেছিলেন কিন্তু কলটি লাগেনি। দয়া করে তাদের আবার ফোন করুন এবং আপনার পিকআপ ঠিক করুন।',
    'BENGALI',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;