-- update atlas_driver_offer_bpp.vehicle_service_tier set stop_fcm_threshold = 0;
-- update atlas_driver_offer_bpp.vehicle_service_tier set stop_fcm_suppress_count = 1;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification
    (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at)
SELECT
    pn.fcm_notification_type,
    pn.key,
    moc.merchant_id,
    moc.id,
    pn.title,
    pn.body,
    pn.language,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    (VALUES
        ('DRIVER_STOP_DETECTED', 'STOP_DETECTION_KEY', 'Customer is waiting!', 'Please start moving towards the pickup location', 'ENGLISH'),
        ('DRIVER_STOP_DETECTED', 'STOP_DETECTION_KEY', 'ग्राहक इंतजार कर रहा है!', 'कृपया पिकअप स्थान की ओर बढ़ना शुरू करें', 'HINDI'),
        ('DRIVER_STOP_DETECTED', 'STOP_DETECTION_KEY', 'ಗ್ರಾಹಕರು ಕಾಯುತ್ತಿದ್ದಾರೆ!', 'ದಯವಿಟ್ಟು ಪಿಕಪ್ ಸ್ಥಳದ ಕಡೆಗೆ ಹೋಗಿ', 'KANNADA'),
        ('DRIVER_STOP_DETECTED', 'STOP_DETECTION_KEY', 'வாடிக்கையாளர் காத்திருக்கிறார்!', 'பிக் அப் இடத்திற்கு செல்லத் தொடங்குங்கள்', 'TAMIL'),
        ('DRIVER_STOP_DETECTED', 'STOP_DETECTION_KEY', 'കസ്റ്റമർ കാത്തിരിക്കുന്നു!', 'ദയവായി പിക്കപ്പ് ലൊക്കേഷനിലേക്ക് നീങ്ങാൻ തുടങ്ങൂ', 'MALAYALAM'),
        ('DRIVER_STOP_DETECTED', 'STOP_DETECTION_KEY', 'కస్టమర్ వెయిట్ చేస్తున్నారు!', 'పికప్ లొకేషన్ వైపు కదలడం ప్రారంభించండి', 'TELUGU'),
        ('DRIVER_STOP_DETECTED', 'STOP_DETECTION_KEY', 'গ্রাহক অপেক্ষা করছেন!', 'দয়া করে পিকআপ লোকেশনের দিকে এগিয়ে যান', 'BENGALI')
    ) AS pn(fcm_notification_type, key, title, body, language),
    atlas_driver_offer_bpp.merchant_operating_city AS moc;
