UPDATE atlas_driver_offer_bpp.driver_information SET payout_vpa_status = 'VIA_WEBHOOK' where payout_vpa is not null;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYOUT_VPA_REMINDER',
    'PAYOUT_VPA_ALERT',
    moc.merchant_id,
    moc.id,
    'Add UPI ID to receive referral bonus of ₹{#rewardAmount#}',
    'Referral bonus transaction pending due to missing UPI ID',
    'ENGLISH',
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
    '₹{#rewardAmount#} Referral Bonus Earned',
    'Referred customer completed their first ride. Keep referring and earn more.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
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
        ('USER_FAVOURITE_DRIVER', 'FAVOURITE_DRIVER_ALERT', '{#riderName#} has added you as their favourite driver!', 'Now, they can schedule regular rides with you, creating new earning opportunities!....', 'ENGLISH'),
        ('USER_FAVOURITE_DRIVER','FAVOURITE_DRIVER_ALERT','{#riderName#} ने आपको अपना पसंदीदा चालक बना लिया है!','अब, वे आपके साथ नियमित सवारी की योजना बना सकते हैं, जिससे नए कमाई के अवसर पैदा होंगे!....','HINDI'),
        ('USER_FAVOURITE_DRIVER','FAVOURITE_DRIVER_ALERT','{#riderName#} ನಿಮಗೆ ತಮ್ಮ ಇಷ್ಟದ ಚಾಲಕ ಎಂದು ಸೇರಿಸಿಕೊಂಡಿದ್ದಾರೆ!','ಈಗ, ಅವರು ನಿಮ್ಮೊಂದಿಗೆ ನಿಯಮಿತ ಸವಾರಿಗಳನ್ನು ನಿಗದಿಪಡಿಸಬಹುದು, ಹೊಸ ಗಳಿಕೆಯ ಅವಕಾಶಗಳನ್ನು ಸೃಷ್ಟಿಸಬಹುದು!....','KANNADA'),
        ('USER_FAVOURITE_DRIVER','FAVOURITE_DRIVER_ALERT','{#riderName#} உங்களை தனது பிடித்த இயக்குநராக சேர்த்துள்ளார்!','இப்போது, அவர்கள் உங்கள் மூலம் வழக்கமான சவாரிகளை திட்டமிடலாம், புதிய வருமான வாய்ப்புகளை உருவாக்குவதற்காக!....','TAMIL'),
        ('USER_FAVOURITE_DRIVER','FAVOURITE_DRIVER_ALERT','{#riderName#} നിങ്ങളെ തന്റെ ഇഷ്ട ഡ്രൈവറായി ചേർത്തിരിക്കുന്നു!','ഇപ്പോൾ, അവർ നിങ്ങളുടെ കൂടെ നിത്യ യാത്രകൾ സംഘടിപ്പിക്കാം, പുതിയ വരുമാന അവസരങ്ങൾ സൃഷ്ടിക്കാൻ!....','MALAYALAM'),
        ('USER_FAVOURITE_DRIVER','FAVOURITE_DRIVER_ALERT','{#riderName#} మీరు తన ఇష్టమైన డ్రైవర్ అని చేర్చుకున్నాడు!','ఇప్పుడు, వారు మీతో రెగ్యులర్ రైడ్‌లను షెడ్యూల్ చేయవచ్చు, కొత్త సంపాదన అవకాశాలను సృష్టించవచ్చు!....','TELUGU'),
        ('USER_FAVOURITE_DRIVER','FAVOURITE_DRIVER_ALERT', '{#riderName#} আপনাকে তার প্রিয় চালক হিসেবে যুক্ত করেছে!','এখন, তারা আপনার সঙ্গে নিয়মিত ভ্রমণের সময় নির্ধারণ করতে পারে, নতুন আয়ের সুযোগ সৃষ্টি করতে পারে!....','BENGALI')
    ) AS pn(fcm_notification_type, key, title, body, language),
    atlas_driver_offer_bpp.merchant_operating_city AS moc;