--  run in master
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
        ('ENGLISH', 'DRUNK_AND_DRIVE_WARNING', '', 'Drunk and Drive Warning', 'Please do not drive under the influence of alcohol.', 'OK'),
        ('HINDI', 'DRUNK_AND_DRIVE_WARNING', '', 'ड्रंक एंड ड्राइव चेतावनी', 'कृपया शराब के प्रभाव में गाड़ी न चलाएं।', 'ठीक है'),
        ('KANNADA', 'DRUNK_AND_DRIVE_WARNING', '', 'ಮದ್ಯಪಾನ ಮತ್ತು ಡ್ರೈವ್ ಎಚ್ಚರಿಕೆ', 'ಮದ್ಯಪಾನ ಮಾಡಿ ವಾಹನ ಓಡಿಸಬೇಡಿ.', 'ಸರಿ'),
        ('TAMIL', 'DRUNK_AND_DRIVE_WARNING', '', 'மது மற்றும் ஓட்டம் எச்சரிக்கை', 'மது அருந்தி வண்டி ஓட்டாதீர்கள்.', 'சரி'),
        ('MALAYALAM', 'DRUNK_AND_DRIVE_WARNING', '', 'മദ്യപാനം & ഡ്രൈവ് മുന്നറിയിപ്പ്', 'മദ്യപിച്ച് വാഹനം ഓടിക്കരുത്.', 'ശരി'),
        ('TELUGU', 'DRUNK_AND_DRIVE_WARNING', '', 'మద్యం మరియు డ్రైవ్ హెచ్చరిక', 'మద్యం తాగి వాహనం నడపవద్దు.', 'సరే'),
        ('BENGALI', 'DRUNK_AND_DRIVE_WARNING', '', 'মদ্যপান এবং ড্রাইভ সতর্কতা', 'মদ্যপান করে গাড়ি চালাবেন না।', 'ঠিক আছে')
    ) AS event(language, overlay_key, image_url, title, description, ok_button_text),
    atlas_driver_offer_bpp.merchant_operating_city AS city;
