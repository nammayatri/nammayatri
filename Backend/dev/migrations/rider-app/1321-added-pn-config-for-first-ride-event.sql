INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FIRST_RIDE_EVENT',
    'FIRST_RIDE_EVENT',
    moc.merchant_id,
    moc.id,
    '🎉 Congrats on your 1st Ride with Namma Yatri !',
    'Your choice supports drivers directly and makes a real difference.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FIRST_RIDE_EVENT',
    'FIRST_RIDE_EVENT',
    moc.merchant_id,
    moc.id,
    '🎉 ನಮ್ಮ ಯಾತ್ರಿಯೊಂದಿಗೆ ನಿಮ್ಮ ಮೊದಲ ರೈಡ್‌ಗೆ ಅಭಿನಂದನೆಗಳು!',
    'ನಿಮ್ಮ ಆಯ್ಕೆಯು ಡ್ರೈವರ್‌ಗಳಿಗೆ ನೇರವಾಗಿ ಬೆಂಬಲಿಸುತ್ತದೆ ಮತ್ತು ನಿಜವಾದ ವ್ಯತ್ಯಾಸವನ್ನು ಮಾಡುತ್ತದೆ.',
    'KANNADA',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FIRST_RIDE_EVENT',
    'FIRST_RIDE_EVENT',
    moc.merchant_id,
    moc.id,
    '🎉 నమ్మ యాత్రితో మీరు మొదటి ప్రయాణం చేసినందుకు అభినందనలు!',
    'మీ ఎంపిక నేరుగా డ్రైవర్లకు మద్దతు ఇస్తుంది మరియు నిజమైన మార్పును తీసుకొస్తుంది.',
    'TELUGU',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FIRST_RIDE_EVENT',
    'FIRST_RIDE_EVENT',
    moc.merchant_id,
    moc.id,
    '🎉 நம்ம யாத்ரியுடன் உங்கள் முதல் பயணத்திற்கு வாழ்த்துக்கள்!',
    'உங்கள் தேர்வு நேரடியாக ஓட்டுனர்களை ஆதரிக்கிறது மற்றும் உண்மையான மாற்றத்தை ஏற்படுத்துகிறது.',
    'TAMIL',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FIRST_RIDE_EVENT',
    'FIRST_RIDE_EVENT',
    moc.merchant_id,
    moc.id,
    '🎉 नम्मा यात्री के साथ आपकी पहली यात्रा के लिए बधाई!',
    'आपकी पसंद सीधे तौर पर ड्राइवरों को सहायता करती है और वास्तविक अंतर लाती है।',
    'HINDI',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;