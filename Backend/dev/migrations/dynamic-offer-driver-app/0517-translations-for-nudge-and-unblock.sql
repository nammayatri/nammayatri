INSERT INTO atlas_driver_offer_bpp.merchant_overlay
    (id, language, overlay_key, image_url, title, description, ok_button_text, merchant_id, merchant_operating_city_id)
SELECT
    md5(random()::text || clock_timestamp()::text || city.id :: text)::uuid,
    event.language,
    event.overlay_key,
    event.image_url,
    event.title,
    event.description,
    event.ok_button_text,
    city.merchant_id,
    city.id
FROM
    (VALUES
        ('ENGLISH', 'CANCELLATION_RATE_NUDGE_DAILY', 'https://assets.moving.tech/beckn/nammayatri/driver/images/ny_ic_frequent_cancellation_blocking.png', 'Your cancellation rate is high!', 'Complete more rides and reduce cancellations to improve your score', 'Ok, Got it'),
        ('HINDI', 'CANCELLATION_RATE_NUDGE_DAILY', 'https://assets.moving.tech/beckn/nammayatri/driver/images/ny_ic_frequent_cancellation_blocking.png', 'आपकी रद्दीकरण दर अधिक है!', 'अपना स्कोर सुधारने के लिए ज़्यादा राइड पूरी करें और रद्दीकरण कम करें', 'ठीक है, समझ गया'),
        ('KANNADA', 'CANCELLATION_RATE_NUDGE_DAILY', 'https://assets.moving.tech/beckn/nammayatri/driver/images/ny_ic_frequent_cancellation_blocking.png', 'ನಿಮ್ಮ ರದ್ದತಿ ದರ ಹೆಚ್ಚಾಗಿದೆ!', 'ನಿಮ್ಮ ಸ್ಕೋರ್ ಅನ್ನು ಸುಧಾರಿಸಲು ಹೆಚ್ಚಿನ ಸವಾರಿಗಳನ್ನು ಪೂರ್ಣಗೊಳಿಸಿ ಮತ್ತು ರದ್ದತಿಗಳನ್ನು ಕಡಿಮೆ ಮಾಡಿ', 'ಸರಿ, ಅರ್ಥವಾಯಿತು'),
        ('TAMIL', 'CANCELLATION_RATE_NUDGE_DAILY', 'https://assets.moving.tech/beckn/nammayatri/driver/images/ny_ic_frequent_cancellation_blocking.png', 'உங்கள் ரத்து விகிதம் அதிகமாக உள்ளது!', 'உங்கள் ஸ்கோரை மேம்படுத்த, அதிகமான சவாரிகளை முடித்து, ரத்துசெய்தல்களைக் குறைக்கவும்', 'சரி, புரிந்தது'),
        ('MALAYALAM', 'CANCELLATION_RATE_NUDGE_DAILY', 'https://assets.moving.tech/beckn/nammayatri/driver/images/ny_ic_frequent_cancellation_blocking.png', 'നിങ്ങളുടെ റദ്ദാക്കൽ നിരക്ക് ഉയർന്നതാണ്!', 'നിങ്ങളുടെ സ്കോർ മെച്ചപ്പെടുത്താൻ കൂടുതൽ റൈഡുകൾ പൂർത്തിയാക്കി റദ്ദാക്കലുകൾ കുറയ്ക്കുക', 'ശരി, മനസ്സിലായി'),
        ('TELUGU', 'CANCELLATION_RATE_NUDGE_DAILY', 'https://assets.moving.tech/beckn/nammayatri/driver/images/ny_ic_frequent_cancellation_blocking.png', 'మీ రద్దు రేటు ఎక్కువగా ఉంది!', 'మీ స్కోర్‌ను మెరుగుపరచడానికి మరిన్ని రైడ్‌లను పూర్తి చేయండి మరియు రద్దులను తగ్గించండి', 'సరే, అర్థమైంది'),
        ('BENGALI', 'CANCELLATION_RATE_NUDGE_DAILY', 'https://assets.moving.tech/beckn/nammayatri/driver/images/ny_ic_frequent_cancellation_blocking.png', 'আপনার বাতিলের হার বেশি!', 'আপনার স্কোর উন্নত করতে আরও রাইড সম্পূর্ণ করুন এবং বাতিলকরণ কম করুন', 'ঠিক আছে, বুঝেছি')
    ) AS event(language, overlay_key, image_url, title, description, ok_button_text),
    atlas_driver_offer_bpp.merchant_operating_city AS city;

INSERT INTO atlas_driver_offer_bpp.merchant_overlay
    (id, language, overlay_key, image_url, title, description, ok_button_text, merchant_id, merchant_operating_city_id)
SELECT
    md5(random()::text || clock_timestamp()::text || city.id :: text)::uuid,
    event.language,
    event.overlay_key,
    event.image_url,
    event.title,
    event.description,
    event.ok_button_text,
    city.merchant_id,
    city.id
FROM
    (VALUES
        ('ENGLISH', 'CANCELLATION_RATE_NUDGE_WEEKLY', 'https://assets.moving.tech/beckn/nammayatri/driver/images/ny_ic_frequent_cancellation_blocking.png', 'Your cancellation rate is high!', 'Complete more rides and reduce cancellations to improve your score', 'Ok, Got it'),
        ('HINDI', 'CANCELLATION_RATE_NUDGE_WEEKLY', 'https://assets.moving.tech/beckn/nammayatri/driver/images/ny_ic_frequent_cancellation_blocking.png', 'आपकी रद्दीकरण दर अधिक है!', 'अपना स्कोर सुधारने के लिए ज़्यादा राइड पूरी करें और रद्दीकरण कम करें', 'ठीक है, समझ गया'),
        ('KANNADA', 'CANCELLATION_RATE_NUDGE_WEEKLY', 'https://assets.moving.tech/beckn/nammayatri/driver/images/ny_ic_frequent_cancellation_blocking.png', 'ನಿಮ್ಮ ರದ್ದತಿ ದರ ಹೆಚ್ಚಾಗಿದೆ!', 'ನಿಮ್ಮ ಸ್ಕೋರ್ ಅನ್ನು ಸುಧಾರಿಸಲು ಹೆಚ್ಚಿನ ಸವಾರಿಗಳನ್ನು ಪೂರ್ಣಗೊಳಿಸಿ ಮತ್ತು ರದ್ದತಿಗಳನ್ನು ಕಡಿಮೆ ಮಾಡಿ', 'ಸರಿ, ಅರ್ಥವಾಯಿತು'),
        ('TAMIL', 'CANCELLATION_RATE_NUDGE_WEEKLY', 'https://assets.moving.tech/beckn/nammayatri/driver/images/ny_ic_frequent_cancellation_blocking.png', 'உங்கள் ரத்து விகிதம் அதிகமாக உள்ளது!', 'உங்கள் ஸ்கோரை மேம்படுத்த, அதிகமான சவாரிகளை முடித்து, ரத்துசெய்தல்களைக் குறைக்கவும்', 'சரி, புரிந்தது'),
        ('MALAYALAM', 'CANCELLATION_RATE_NUDGE_WEEKLY', 'https://assets.moving.tech/beckn/nammayatri/driver/images/ny_ic_frequent_cancellation_blocking.png', 'നിങ്ങളുടെ റദ്ദാക്കൽ നിരക്ക് ഉയർന്നതാണ്!', 'നിങ്ങളുടെ സ്കോർ മെച്ചപ്പെടുത്താൻ കൂടുതൽ റൈഡുകൾ പൂർത്തിയാക്കി റദ്ദാക്കലുകൾ കുറയ്ക്കുക', 'ശരി, മനസ്സിലായി'),
        ('TELUGU', 'CANCELLATION_RATE_NUDGE_WEEKLY', 'https://assets.moving.tech/beckn/nammayatri/driver/images/ny_ic_frequent_cancellation_blocking.png', 'మీ రద్దు రేటు ఎక్కువగా ఉంది!', 'మీ స్కోర్‌ను మెరుగుపరచడానికి మరిన్ని రైడ్‌లను పూర్తి చేయండి మరియు రద్దులను తగ్గించండి', 'సరే, అర్థమైంది'),
        ('BENGALI', 'CANCELLATION_RATE_NUDGE_WEEKLY', 'https://assets.moving.tech/beckn/nammayatri/driver/images/ny_ic_frequent_cancellation_blocking.png', 'আপনার বাতিলের হার বেশি!', 'আপনার স্কোর উন্নত করতে আরও রাইড সম্পূর্ণ করুন এবং বাতিলকরণ কম করুন', 'ঠিক আছে, বুঝেছি')
    ) AS event(language, overlay_key, image_url, title, description, ok_button_text),
    atlas_driver_offer_bpp.merchant_operating_city AS city;