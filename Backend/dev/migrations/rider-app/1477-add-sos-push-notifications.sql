-- SOS Push Notification Templates
-- Adds configurable push notification templates for SOS scenarios with multi-language support

-- SOS_ALERT (ENGLISH)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_TRIGGERED', 'SOS_ALERT', moc.merchant_id, moc.id, 'SOS Alert', '{#userName#} has activated SOS. Tap to track and respond to the emergency situation.', 'ENGLISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- SOS_ALERT (HINDI)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_TRIGGERED', 'SOS_ALERT', moc.merchant_id, moc.id, 'SOS अलर्ट', '{#userName#} ने SOS सक्रिय किया है। ट्रैक करने और आपातकालीन स्थिति पर प्रतिक्रिया देने के लिए टैप करें।', 'HINDI', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- SOS_ALERT (KANNADA)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_TRIGGERED', 'SOS_ALERT', moc.merchant_id, moc.id, 'SOS ಎಚ್ಚರಿಕೆ', '{#userName#} SOS ಅನ್ನು ಸಕ್ರಿಯಗೊಳಿಸಿದ್ದಾರೆ. ಟ್ರ್ಯಾಕ್ ಮಾಡಲು ಮತ್ತು ತುರ್ತು ಪರಿಸ್ಥಿತಿಗೆ ಪ್ರತಿಕ್ರಿಯಿಸಲು ಟ್ಯಾಪ್ ಮಾಡಿ।', 'KANNADA', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- SOS_ALERT (TAMIL)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_TRIGGERED', 'SOS_ALERT', moc.merchant_id, moc.id, 'SOS எச்சரிக்கை', '{#userName#} SOS ஐ செயல்படுத்தியுள்ளார். கண்காணிக்கவும் மற்றும் அவசரகால சூழ்நிலைக்கு பதிலளிக்கவும் தட்டவும்।', 'TAMIL', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- SOS_ALERT (TELUGU)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_TRIGGERED', 'SOS_ALERT', moc.merchant_id, moc.id, 'SOS హెచ్చరిక', '{#userName#} SOS ని సక్రియం చేసారు. ట్రాక్ చేయడానికి మరియు అత్యవసర పరిస్థితికి ప్రతిస్పందించడానికి ట్యాప్ చేయండి।', 'TELUGU', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- SOS_ALERT (MALAYALAM)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_TRIGGERED', 'SOS_ALERT', moc.merchant_id, moc.id, 'SOS അലേർട്ട്', '{#userName#} SOS സജീവമാക്കി. ട്രാക്ക് ചെയ്യാനും അടിയന്തിര സാഹചര്യത്തിന് പ്രതികരിക്കാനും ടാപ്പ് ചെയ്യുക।', 'MALAYALAM', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- SOS_ALERT (BENGALI)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_TRIGGERED', 'SOS_ALERT', moc.merchant_id, moc.id, 'SOS সতর্কতা', '{#userName#} SOS সক্রিয় করেছেন। ট্র্যাক করতে এবং জরুরি পরিস্থিতিতে সাড়া দিতে ট্যাপ করুন।', 'BENGALI', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- SOS_RESOLVED_SAFE (ENGLISH)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_RESOLVED', 'SOS_RESOLVED_SAFE', moc.merchant_id, moc.id, 'Marked Safe', '{#userName#} is safe now. Tap to view the details.', 'ENGLISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- SOS_RESOLVED_SAFE (HINDI)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_RESOLVED', 'SOS_RESOLVED_SAFE', moc.merchant_id, moc.id, 'सुरक्षित चिह्नित', '{#userName#} अब सुरक्षित हैं। विवरण देखने के लिए टैप करें।', 'HINDI', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- SOS_RESOLVED_SAFE (KANNADA)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_RESOLVED', 'SOS_RESOLVED_SAFE', moc.merchant_id, moc.id, 'ಸುರಕ್ಷಿತವಾಗಿ ಗುರುತಿಸಲಾಗಿದೆ', '{#userName#} ಈಗ ಸುರಕ್ಷಿತರಾಗಿದ್ದಾರೆ. ವಿವರಗಳನ್ನು ವೀಕ್ಷಿಸಲು ಟ್ಯಾಪ್ ಮಾಡಿ।', 'KANNADA', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- SOS_RESOLVED_SAFE (TAMIL)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_RESOLVED', 'SOS_RESOLVED_SAFE', moc.merchant_id, moc.id, 'பாதுகாப்பாகக் குறிக்கப்பட்டது', '{#userName#} இப்போது பாதுகாப்பாக உள்ளார். விவரங்களைப் பார்க்க தட்டவும்।', 'TAMIL', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- SOS_RESOLVED_SAFE (TELUGU)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_RESOLVED', 'SOS_RESOLVED_SAFE', moc.merchant_id, moc.id, 'సురక్షితంగా గుర్తించబడింది', '{#userName#} ఇప్పుడు సురక్షితంగా ఉన్నారు. వివరాలను వీక్షించడానికి ట్యాప్ చేయండి।', 'TELUGU', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- SOS_RESOLVED_SAFE (MALAYALAM)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_RESOLVED', 'SOS_RESOLVED_SAFE', moc.merchant_id, moc.id, 'സുരക്ഷിതമായി അടയാളപ്പെടുത്തി', '{#userName#} ഇപ്പോൾ സുരക്ഷിതമാണ്. വിശദാംശങ്ങൾ കാണാൻ ടാപ്പ് ചെയ്യുക।', 'MALAYALAM', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- SOS_RESOLVED_SAFE (BENGALI)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_RESOLVED', 'SOS_RESOLVED_SAFE', moc.merchant_id, moc.id, 'নিরাপদ হিসাবে চিহ্নিত', '{#userName#} এখন নিরাপদ। বিস্তারিত দেখতে ট্যাপ করুন।', 'BENGALI', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- LIVE_TRACKING_STARTED (ENGLISH)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SHARE_RIDE', 'LIVE_TRACKING_STARTED', moc.merchant_id, moc.id, 'Location Shared', '{#userName#} has shared their location with you. Tap to track their live location.', 'ENGLISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- LIVE_TRACKING_STARTED (HINDI)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SHARE_RIDE', 'LIVE_TRACKING_STARTED', moc.merchant_id, moc.id, 'स्थान साझा किया गया', '{#userName#} ने आपके साथ अपना स्थान साझा किया है। उनके लाइव स्थान को ट्रैक करने के लिए टैप करें।', 'HINDI', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- LIVE_TRACKING_STARTED (KANNADA)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SHARE_RIDE', 'LIVE_TRACKING_STARTED', moc.merchant_id, moc.id, 'ಸ್ಥಳ ಹಂಚಿಕೊಳ್ಳಲಾಗಿದೆ', '{#userName#} ಅವರ ಸ್ಥಳವನ್ನು ನಿಮ್ಮೊಂದಿಗೆ ಹಂಚಿಕೊಂಡಿದ್ದಾರೆ। ಅವರ ಲೈವ್ ಸ್ಥಳವನ್ನು ಟ್ರ್ಯಾಕ್ ಮಾಡಲು ಟ್ಯಾಪ್ ಮಾಡಿ।', 'KANNADA', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- LIVE_TRACKING_STARTED (TAMIL)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SHARE_RIDE', 'LIVE_TRACKING_STARTED', moc.merchant_id, moc.id, 'இடம் பகிரப்பட்டது', '{#userName#} தங்கள் இடத்தை உங்களுடன் பகிர்ந்து கொண்டுள்ளார்। அவர்களின் நேரடி இடத்தை கண்காணிக்க தட்டவும்।', 'TAMIL', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- LIVE_TRACKING_STARTED (TELUGU)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SHARE_RIDE', 'LIVE_TRACKING_STARTED', moc.merchant_id, moc.id, 'స్థానం భాగస్వామ్యం చేయబడింది', '{#userName#} వారి స్థానాన్ని మీతో భాగస్వామ్యం చేశారు। వారి లైవ్ స్థానాన్ని ట్రాక్ చేయడానికి ట్యాప్ చేయండి।', 'TELUGU', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- LIVE_TRACKING_STARTED (MALAYALAM)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SHARE_RIDE', 'LIVE_TRACKING_STARTED', moc.merchant_id, moc.id, 'സ്ഥാനം പങ്കിട്ടു', '{#userName#} അവരുടെ സ്ഥാനം നിങ്ങളുമായി പങ്കിട്ടു। അവരുടെ ലൈവ് സ്ഥാനം ട്രാക്ക് ചെയ്യാൻ ടാപ്പ് ചെയ്യുക।', 'MALAYALAM', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- LIVE_TRACKING_STARTED (BENGALI)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SHARE_RIDE', 'LIVE_TRACKING_STARTED', moc.merchant_id, moc.id, 'অবস্থান শেয়ার করা হয়েছে', '{#userName#} তাদের অবস্থান আপনার সাথে শেয়ার করেছেন। তাদের লাইভ অবস্থান ট্র্যাক করতে ট্যাপ করুন।', 'BENGALI', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- LIVE_TRACKING_STOPPED (ENGLISH)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_RESOLVED', 'LIVE_TRACKING_STOPPED', moc.merchant_id, moc.id, 'Location Sharing Stopped', '{#userName#} has stopped sharing their location with you.', 'ENGLISH', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- LIVE_TRACKING_STOPPED (HINDI)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_RESOLVED', 'LIVE_TRACKING_STOPPED', moc.merchant_id, moc.id, 'स्थान साझाकरण बंद कर दिया गया', '{#userName#} ने आपके साथ अपना स्थान साझा करना बंद कर दिया है।', 'HINDI', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- LIVE_TRACKING_STOPPED (KANNADA)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_RESOLVED', 'LIVE_TRACKING_STOPPED', moc.merchant_id, moc.id, 'ಸ್ಥಳ ಹಂಚಿಕೆ ನಿಲ್ಲಿಸಲಾಗಿದೆ', '{#userName#} ನಿಮ್ಮೊಂದಿಗೆ ತಮ್ಮ ಸ್ಥಳವನ್ನು ಹಂಚಿಕೊಳ್ಳುವುದನ್ನು ನಿಲ್ಲಿಸಿದ್ದಾರೆ।', 'KANNADA', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- LIVE_TRACKING_STOPPED (TAMIL)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_RESOLVED', 'LIVE_TRACKING_STOPPED', moc.merchant_id, moc.id, 'இடம் பகிர்வு நிறுத்தப்பட்டது', '{#userName#} தங்கள் இடத்தை உங்களுடன் பகிர்வதை நிறுத்தியுள்ளார்।', 'TAMIL', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- LIVE_TRACKING_STOPPED (TELUGU)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_RESOLVED', 'LIVE_TRACKING_STOPPED', moc.merchant_id, moc.id, 'స్థానం భాగస్వామ్యం ఆపబడింది', '{#userName#} వారి స్థానాన్ని మీతో భాగస్వామ్యం చేయడం ఆపారు।', 'TELUGU', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- LIVE_TRACKING_STOPPED (MALAYALAM)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_RESOLVED', 'LIVE_TRACKING_STOPPED', moc.merchant_id, moc.id, 'സ്ഥാനം പങ്കിടൽ നിർത്തി', '{#userName#} അവരുടെ സ്ഥാനം നിങ്ങളുമായി പങ്കിടുന്നത് നിർത്തി।', 'MALAYALAM', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;

-- LIVE_TRACKING_STOPPED (BENGALI)
INSERT INTO atlas_app.merchant_push_notification (fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at) SELECT 'SOS_RESOLVED', 'LIVE_TRACKING_STOPPED', moc.merchant_id, moc.id, 'অবস্থান শেয়ারিং বন্ধ করা হয়েছে', '{#userName#} তাদের অবস্থান আপনার সাথে শেয়ার করা বন্ধ করেছেন।', 'BENGALI', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP FROM atlas_app.merchant_operating_city moc ON CONFLICT DO NOTHING;
