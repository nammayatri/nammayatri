-- NOTE: need to delete first to remove old entries

DELETE from atlas_driver_offer_bpp.merchant_overlay
WHERE overlay_key = 'EDIT_LOCATION';

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
        ('ENGLISH', 'EDIT_LOCATION', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_pickup_location_updated.png', 'Pickup location has been updated!', 'Customer has changed pickup location. Please check the app for more details', 'Update Navigation'),
        ('HINDI', 'EDIT_LOCATION', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_pickup_location_updated.png', 'पिकअप स्थान अपडेट कर दिया गया है!', 'ग्राहक ने पिकअप स्थान बदल दिया है. अधिक जानकारी के लिए कृपया ऐप जांचें', 'नेविगेशन अपडेट करें'),
        ('KANNADA', 'EDIT_LOCATION', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_pickup_location_updated.png', 'ಪಿಕಪ್ ಸ್ಥಳವನ್ನು ನವೀಕರಿಸಲಾಗಿದೆ!', 'ಗ್ರಾಹಕರು ಪಿಕಪ್ ಸ್ಥಳವನ್ನು ಬದಲಾಯಿಸಿದ್ದಾರೆ. ಹೆಚ್ಚಿನ ವಿವರಗಳಿಗಾಗಿ ದಯವಿಟ್ಟು ಅಪ್ಲಿಕೇಶನ್ ಅನ್ನು ಪರಿಶೀಲಿಸಿ', 'ನ್ಯಾವಿಗೇಶನ್ ಅನ್ನು ನವೀಕರಿಸಿ'),
        ('TAMIL', 'EDIT_LOCATION', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_pickup_location_updated.png', 'பிக்-அப் இடம் புதுப்பிக்கப்பட்டது!', 'வாடிக்கையாளர் பிக்அப் இடத்தை மாற்றியுள்ளார். மேலும் விவரங்களுக்கு பயன்பாட்டைச் சரிபார்க்கவும்', 'வழிசெலுத்தலைப் புதுப்பிக்கவும்'),
        ('MALAYALAM', 'EDIT_LOCATION', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_pickup_location_updated.png', 'പിക്കപ്പ് ലൊക്കേഷൻ അപ്ഡേറ്റ് ചെയ്തു!', 'ഉപഭോക്താവ് പിക്കപ്പ് ലൊക്കേഷൻ മാറ്റി. കൂടുതൽ വിവരങ്ങൾക്ക് ആപ്പ് പരിശോധിക്കുക', 'നാവിഗേഷൻ അപ്ഡേറ്റ് ചെയ്യുക'),
        ('TELUGU', 'EDIT_LOCATION', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_pickup_location_updated.png', 'పికప్ లొకేషన్ అప్‌డేట్ చేయబడింది!', 'కస్టమర్ పికప్ స్థానాన్ని మార్చారు. దయచేసి మరిన్ని వివరాల కోసం యాప్‌ని తనిఖీ చేయండి', 'నావిగేషన్‌ని నవీకరించండి'),
        ('BENGALI', 'EDIT_LOCATION', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_pickup_location_updated.png', 'পিকআপ অবস্থান আপডেট করা হয়েছে!', 'গ্রাহক পিকআপ অবস্থান পরিবর্তন করেছেন। আরো বিস্তারিত জানার জন্য অ্যাপ চেক করুন', 'নেভিগেশন আপডেট করুন')
    ) AS event(language, overlay_key, image_url, title, description, ok_button_text),
    atlas_driver_offer_bpp.merchant_operating_city AS city;