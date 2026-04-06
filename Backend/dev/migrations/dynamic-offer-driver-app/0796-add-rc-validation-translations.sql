-- RC validation failure translations (multilingual)
-- Keys used in VehicleRegistrationCertificate.validateRCResponse
--   InvalidFuelType, InvalidVehicleClass, InvalidOEM, InvalidManufacturingYear

WITH translation_seed AS (
  SELECT * FROM (
    VALUES
      ('InvalidFuelType', 'ENGLISH', 'Vehicles with {#fuelType#} type are not allowed for this service.'),
      ('InvalidVehicleClass', 'ENGLISH', 'Vehicles like {#vehicleClass#} are not allowed for this service.'),
      ('InvalidOEM', 'ENGLISH', 'Vehicle manufacturer/model {#manufacturer#} is not supported for this service.'),
      ('InvalidManufacturingYear', 'ENGLISH', 'Vehicle manufacturing year {#year#} is not eligible (vehicle is older than the allowed limit).'),

      ('InvalidFuelType', 'HINDI', 'इस {#fuelType#} ईंधन प्रकार वाले वाहन इस सेवा के लिए अनुमत नहीं हैं।'),
      ('InvalidVehicleClass', 'HINDI', '{#vehicleClass#} वर्ग के वाहन इस सेवा के लिए अनुमत नहीं हैं।'),
      ('InvalidOEM', 'HINDI', 'इस सेवा के लिए वाहन निर्माता/मॉडल {#manufacturer#} समर्थित नहीं है।'),
      ('InvalidManufacturingYear', 'HINDI', 'वाहन का निर्माण वर्ष {#year#} पात्र नहीं है (वाहन अनुमत सीमा से अधिक पुराना है)।'),

      ('InvalidFuelType', 'KANNADA', '{#fuelType#} ಇಂಧನ ಪ್ರಕಾರದ ವಾಹನಗಳು ಈ ಸೇವೆಗೆ ಅನುಮತಿಸಲಾಗುವುದಿಲ್ಲ.'),
      ('InvalidVehicleClass', 'KANNADA', '{#vehicleClass#} ವರ್ಗದ ವಾಹನಗಳಿಗೆ ಈ ಸೇವೆಯಲ್ಲಿ ಅನುಮತಿ ಇಲ್ಲ.'),
      ('InvalidOEM', 'KANNADA', 'ಈ ಸೇವೆಗೆ ವಾಹನ ತಯಾರಕರು/ಮಾದರಿ {#manufacturer#} ಬೆಂಬಲಿತವಲ್ಲ.'),
      ('InvalidManufacturingYear', 'KANNADA', 'ವಾಹನದ ತಯಾರಿಕಾ ವರ್ಷ {#year#} ಅರ್ಹವಲ್ಲ (ವಾಹನ ಅನುಮತಿಸಲಾದ ಮಿತಿಗಿಂತ ಹಳೆಯದು).'),

      ('InvalidFuelType', 'TAMIL', '{#fuelType#} எரிபொருள் வகையுள்ள வாகனங்கள் இந்த சேவைக்கு அனுமதிக்கப்படவில்லை.'),
      ('InvalidVehicleClass', 'TAMIL', '{#vehicleClass#} வகை வாகனங்கள் இந்த சேவைக்கு அனுமதிக்கப்படவில்லை.'),
      ('InvalidOEM', 'TAMIL', 'இந்த சேவைக்கு வாகன உற்பத்தியாளர்/மாதிரி {#manufacturer#} ஆதரிக்கப்படவில்லை.'),
      ('InvalidManufacturingYear', 'TAMIL', 'வாகன உற்பத்தி ஆண்டு {#year#} தகுதி பெறவில்லை (அனுமதிக்கப்பட்ட வரம்பை விட வாகனம் பழையது).'),

      ('InvalidFuelType', 'TELUGU', '{#fuelType#} ఇంధన రకం వాహనాలు ఈ సేవకు అనుమతించబడవు.'),
      ('InvalidVehicleClass', 'TELUGU', '{#vehicleClass#} తరగతికి చెందిన వాహనాలు ఈ సేవకు అనుమతించబడవు.'),
      ('InvalidOEM', 'TELUGU', 'ఈ సేవకు వాహన తయారీదారు/మోడల్ {#manufacturer#}కు మద్దతు లేదు.'),
      ('InvalidManufacturingYear', 'TELUGU', 'వాహన తయారీ సంవత్సరం {#year#} అర్హత లేదు (అనుమతించిన పరిమితికి మించి వాహనం పాతది).'),

      ('InvalidFuelType', 'MALAYALAM', '{#fuelType#} ഇന്ധന തരം വാഹനങ്ങൾ ഈ സേവനത്തിന് അനുവദനീയമല്ല.'),
      ('InvalidVehicleClass', 'MALAYALAM', '{#vehicleClass#} വിഭാഗത്തിലുള്ള വാഹനങ്ങൾ ഈ സേവനത്തിന് അനുവദനീയമല്ല.'),
      ('InvalidOEM', 'MALAYALAM', 'ഈ സേവനത്തിന് വാഹന നിർമാതാവ്/മോഡൽ {#manufacturer#} പിന്തുണയ്ക്കുന്നില്ല.'),
      ('InvalidManufacturingYear', 'MALAYALAM', 'വാഹന നിർമ്മാണ വർഷം {#year#} യോഗ്യമല്ല (അനുവദിച്ച പരിധിയെക്കാൾ വാഹനം പഴയതാണ്).'),

      ('InvalidFuelType', 'BENGALI', '{#fuelType#} জ্বালানি ধরনের যানবাহন এই পরিষেবার জন্য অনুমোদিত নয়।'),
      ('InvalidVehicleClass', 'BENGALI', '{#vehicleClass#} শ্রেণির যানবাহন এই পরিষেবার জন্য অনুমোদিত নয়।'),
      ('InvalidOEM', 'BENGALI', 'এই পরিষেবার জন্য যানবাহনের নির্মাতা/মডেল {#manufacturer#} সমর্থিত নয়।'),
      ('InvalidManufacturingYear', 'BENGALI', 'যানবাহনের নির্মাণ সাল {#year#} যোগ্য নয় (অনুমোদিত সীমার চেয়ে পুরনো)।')
  ) AS t(message_key, language, message)
)
UPDATE atlas_driver_offer_bpp.translations tr
SET
  message = s.message,
  updated_at = now()
FROM translation_seed s
WHERE tr.message_key = s.message_key
  AND tr.language = s.language;

INSERT INTO atlas_driver_offer_bpp.translations (id, message_key, language, message, created_at, updated_at)
WITH translation_seed AS (
  SELECT * FROM (
    VALUES
      ('InvalidFuelType', 'ENGLISH', 'Vehicles with {#fuelType#} type are not allowed for this service.'),
      ('InvalidVehicleClass', 'ENGLISH', 'Vehicles like {#vehicleClass#} are not allowed for this service.'),
      ('InvalidOEM', 'ENGLISH', 'Vehicle manufacturer/model {#manufacturer#} is not supported for this service.'),
      ('InvalidManufacturingYear', 'ENGLISH', 'Vehicle manufacturing year {#year#} is not eligible (vehicle is older than the allowed limit).'),

      ('InvalidFuelType', 'HINDI', 'इस {#fuelType#} ईंधन प्रकार वाले वाहन इस सेवा के लिए अनुमत नहीं हैं।'),
      ('InvalidVehicleClass', 'HINDI', '{#vehicleClass#} वर्ग के वाहन इस सेवा के लिए अनुमत नहीं हैं।'),
      ('InvalidOEM', 'HINDI', 'इस सेवा के लिए वाहन निर्माता/मॉडल {#manufacturer#} समर्थित नहीं है।'),
      ('InvalidManufacturingYear', 'HINDI', 'वाहन का निर्माण वर्ष {#year#} पात्र नहीं है (वाहन अनुमत सीमा से अधिक पुराना है)।'),

      ('InvalidFuelType', 'KANNADA', '{#fuelType#} ಇಂಧನ ಪ್ರಕಾರದ ವಾಹನಗಳು ಈ ಸೇವೆಗೆ ಅನುಮತಿಸಲಾಗುವುದಿಲ್ಲ.'),
      ('InvalidVehicleClass', 'KANNADA', '{#vehicleClass#} ವರ್ಗದ ವಾಹನಗಳಿಗೆ ಈ ಸೇವೆಯಲ್ಲಿ ಅನುಮತಿ ಇಲ್ಲ.'),
      ('InvalidOEM', 'KANNADA', 'ಈ ಸೇವೆಗೆ ವಾಹನ ತಯಾರಕರು/ಮಾದರಿ {#manufacturer#} ಬೆಂಬಲಿತವಲ್ಲ.'),
      ('InvalidManufacturingYear', 'KANNADA', 'ವಾಹನದ ತಯಾರಿಕಾ ವರ್ಷ {#year#} ಅರ್ಹವಲ್ಲ (ವಾಹನ ಅನುಮತಿಸಲಾದ ಮಿತಿಗಿಂತ ಹಳೆಯದು).'),

      ('InvalidFuelType', 'TAMIL', '{#fuelType#} எரிபொருள் வகையுள்ள வாகனங்கள் இந்த சேவைக்கு அனுமதிக்கப்படவில்லை.'),
      ('InvalidVehicleClass', 'TAMIL', '{#vehicleClass#} வகை வாகனங்கள் இந்த சேவைக்கு அனுமதிக்கப்படவில்லை.'),
      ('InvalidOEM', 'TAMIL', 'இந்த சேவைக்கு வாகன உற்பத்தியாளர்/மாதிரி {#manufacturer#} ஆதரிக்கப்படவில்லை.'),
      ('InvalidManufacturingYear', 'TAMIL', 'வாகன உற்பத்தி ஆண்டு {#year#} தகுதி பெறவில்லை (அனுமதிக்கப்பட்ட வரம்பை விட வாகனம் பழையது).'),

      ('InvalidFuelType', 'TELUGU', '{#fuelType#} ఇంధన రకం వాహనాలు ఈ సేవకు అనుమతించబడవు.'),
      ('InvalidVehicleClass', 'TELUGU', '{#vehicleClass#} తరగతికి చెందిన వాహనాలు ఈ సేవకు అనుమతించబడవు.'),
      ('InvalidOEM', 'TELUGU', 'ఈ సేవకు వాహన తయారీదారు/మోడల్ {#manufacturer#}కు మద్దతు లేదు.'),
      ('InvalidManufacturingYear', 'TELUGU', 'వాహన తయారీ సంవత్సరం {#year#} అర్హత లేదు (అనుమతించిన పరిమితికి మించి వాహనం పాతది).'),

      ('InvalidFuelType', 'MALAYALAM', '{#fuelType#} ഇന്ധന തരം വാഹനങ്ങൾ ഈ സേവനത്തിന് അനുവദനീയമല്ല.'),
      ('InvalidVehicleClass', 'MALAYALAM', '{#vehicleClass#} വിഭാഗത്തിലുള്ള വാഹനങ്ങൾ ഈ സേവനത്തിന് അനുവദനീയമല്ല.'),
      ('InvalidOEM', 'MALAYALAM', 'ഈ സേവനത്തിന് വാഹന നിർമാതാവ്/മോഡൽ {#manufacturer#} പിന്തുണയ്ക്കുന്നില്ല.'),
      ('InvalidManufacturingYear', 'MALAYALAM', 'വാഹന നിർമ്മാണ വർഷം {#year#} യോഗ്യമല്ല (അനുവദിച്ച പരിധിയെക്കാൾ വാഹനം പഴയതാണ്).'),

      ('InvalidFuelType', 'BENGALI', '{#fuelType#} জ্বালানি ধরনের যানবাহন এই পরিষেবার জন্য অনুমোদিত নয়।'),
      ('InvalidVehicleClass', 'BENGALI', '{#vehicleClass#} শ্রেণির যানবাহন এই পরিষেবার জন্য অনুমোদিত নয়।'),
      ('InvalidOEM', 'BENGALI', 'এই পরিষেবার জন্য যানবাহনের নির্মাতা/মডেল {#manufacturer#} সমর্থিত নয়।'),
      ('InvalidManufacturingYear', 'BENGALI', 'যানবাহনের নির্মাণ সাল {#year#} যোগ্য নয় (অনুমোদিত সীমার চেয়ে পুরনো)।')
  ) AS t(message_key, language, message)
)
SELECT
  atlas_driver_offer_bpp.uuid_generate_v4(),
  s.message_key,
  s.language,
  s.message,
  now(),
  now()
FROM translation_seed s
WHERE NOT EXISTS (
  SELECT 1
  FROM atlas_driver_offer_bpp.translations tr
  WHERE tr.message_key = s.message_key
    AND tr.language = s.language
);
