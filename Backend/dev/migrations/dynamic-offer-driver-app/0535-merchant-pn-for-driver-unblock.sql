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
        ('DRIVER_UNBLOCKED', 'UNBLOCK_DRIVER_KEY', 'You are now unblocked', 'Enjoy your commission free rides!', 'ENGLISH'),
        ('DRIVER_UNBLOCKED','UNBLOCK_DRIVER_KEY','अब आप अनब्लॉक हो चुके हैं','अपने बिना कमीशन वाली सवारी का आनंद लें!','HINDI'),
        ('DRIVER_UNBLOCKED','UNBLOCK_DRIVER_KEY','ನೀವು ಈಗ ಅನ್‌ಬ್ಲಾಕ್ ಆಗಿದ್ದೀರಿ','ನಿಮ್ಮ ಕಮಿಷನ್ ಉಚಿತ ರೈಡ್‌ಗಳನ್ನು ಆನಂದಿಸಿ!','KANNADA'),
        ('DRIVER_UNBLOCKED','UNBLOCK_DRIVER_KEY','இப்பொழுது உங்கள் தடைகள் நீக்கப்பட்டது','உங்கள் கமிஷன் இல்லாத பயணத்தை அனுபவிக்கவும்!','TAMIL'),
        ('DRIVER_UNBLOCKED','UNBLOCK_DRIVER_KEY','നിങ്ങൾ ഇപ്പോൾ അൺബ്ലോക്ക് ചെയ്തിരിക്കുന്നു','നിങ്ങളുടെ കമ്മീഷൻ രഹിത യാത്രകൾ ആസ്വദിക്കൂ!','MALAYALAM'),
        ('DRIVER_UNBLOCKED','UNBLOCK_DRIVER_KEY','మీరు ఇప్పుడు అన్‌బ్లాక్ అయ్యారు','మీ కమిషన్ లేని రైడ్‌లను ఆనందించండి!','TELUGU'),
        ('DRIVER_UNBLOCKED','UNBLOCK_DRIVER_KEY', 'আপনি এখন আনব্লকড হয়েগেছেন','আপনার কমিশন মুক্ত যাত্রা উপভোগ করুন!','BENGALI')
    ) AS pn(fcm_notification_type, key, title, body, language),
    atlas_driver_offer_bpp.merchant_operating_city AS moc;