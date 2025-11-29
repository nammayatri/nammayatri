-- ============================================================================
-- FEEDBACK FORM MIGRATION - CITY-SPECIFIC FORMS
-- ============================================================================
-- This migration creates feedback forms for merchant operating cities
-- Each form template is replicated for every city in the system
-- Forms are grouped by category and rating, with multiple badges per form
--
-- STATE-SPECIFIC FORMS:
-- - West Bengal has different badges for ratings 1, 2, 3, 4, 5
-- - All other states use standard badges migrated from frontend
-- - Only ratings 1-5 are included in this migration
-- ============================================================================

-- ============================================================================
-- DRIVER CATEGORY - Rating 1 (Very Poor Experience) - EXCLUDING WEST BENGAL
-- ============================================================================

INSERT INTO atlas_app.feedback_form (
    id, category_name, rating, question, question_translations, answer, answer_type,
    merchant_operating_city_id, merchant_id, badges
)
SELECT
    md5(random()::text || clock_timestamp()::text)::uuid as id,
    'DRIVER', 1,
    'Sorry for your poor experience. Feel free to let us know your feedback about the driver.',
    '[{"language": "ENGLISH", "translation": "Sorry for your poor experience. Feel free to let us know your feedback about the driver."}, {"language": "HINDI", "translation": "आपके ख़राब अनुभव के लिए क्षमा करें। बेझिझक हमें ड्राइवर के बारे में अपनी प्रतिक्रिया बताएं।"}, {"language": "KANNADA", "translation": "ನಿಮ್ಮ ಕಳಪೆ ಅನುಭವಕ್ಕಾಗಿ ಕ್ಷಮಿಸಿ.ಚಾಲಕನ ಬಗ್ಗೆ ನಿಮ್ಮ ಪ್ರತಿಕ್ರಿಯೆಯನ್ನು ನಮಗೆ ತಿಳಿಸಲು ಹಿಂಜರಿಯಬೇಡಿ."}, {"language": "TAMIL", "translation": "உங்கள் மோசமான அனுபவத்திற்கு மன்னிக்கவும்.ஓட்டுனர் பற்றிய உங்கள் கருத்தை எங்களுக்குத் தெரியப்படுத்துங்கள்."}, {"language": "TELUGU", "translation": "మీ అసౌకర్యానికి చింతిస్తున్నాం. డ్రైవర్ గురించి మీ అభిప్రాయాన్ని మాకు తెలియజేయడానికి సంకోచించకండి."}, {"language": "BENGALI", "translation": "আপনার খারাপ অভিজ্ঞতা জন্য দুঃখিত. ড্রাইভার সম্পর্কে আপনার মতামত আমাদের জানাতে নির্দ্বিধায়।"}, {"language": "ODIA", "translation": "ଆପଣଙ୍କର ଖରାପ ଅନୁଭବ ପାଇଁ ଦୁଃଖିତ। ଡ୍ରାଇଭର ବିଷୟରେ ଆପଣଙ୍କର ପ୍ରତିକ୍ରିୟା ଦେବାକୁ ସ୍ୱାଗତ"}]'::json,
    ARRAY['Rude driver', 'Felt unsafe', 'Reckless driving', 'Driver charged more', 'Trip delayed']::text[],
    'Checkbox',
    moc.id, moc.merchant_id,
    '[
        {
            "key": "RUDE_DRIVER",
            "sendPN": true,
            "priority": 1,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Rude driver"},
                {"language": "HINDI", "translation": "असभ्य चालक"},
                {"language": "KANNADA", "translation": "ಅಸಭ್ಯ ಚಾಲಕ"},
                {"language": "TAMIL", "translation": "முரட்டுத்தனமான ஓட்டுனர்"},
                {"language": "TELUGU", "translation": "మొరటు డ్రైవర్"},
                {"language": "BENGALI", "translation": "অভদ্র ড্রাইভার"},
                {"language": "ODIA", "translation": "ଅସଭ୍ୟ ଡ୍ରାଇଭର"}
            ]
        },
        {
            "key": "FELT_UNSAFE",
            "sendPN": false,
            "priority": 2,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Felt unsafe"},
                {"language": "HINDI", "translation": "असुरक्षित लगा"},
                {"language": "KANNADA", "translation": "ಅಸುರಕ್ಷಿತ ಭಾವನೆ"},
                {"language": "TAMIL", "translation": "பாதுகாப்பற்றதாக உணர்ந்தேன்"},
                {"language": "TELUGU", "translation": "అసురక్షితంగా అనిపించింది"},
                {"language": "BENGALI", "translation": "অনিরাপদ বোধ করছেন"},
                {"language": "ODIA", "translation": "ଅସୁରକ୍ଷିତ ମନେ ହେଲା"}
            ]
        },
        {
            "key": "RECKLESS_DRIVING",
            "sendPN": false,
            "priority": 3,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Reckless driving"},
                {"language": "HINDI", "translation": "लापरवाह ड्राइविंग"},
                {"language": "KANNADA", "translation": "ಅಜಾಗರೂಕ ಚಾಲನೆ"},
                {"language": "TAMIL", "translation": "பொறுப்பற்ற வாகனம் ஓட்டுதல்"},
                {"language": "TELUGU", "translation": "నిర్లక్ష్యంగా డ్రైవింగ్"},
                {"language": "BENGALI", "translation": "বেপরোয়া গাড়ি চালানো"},
                {"language": "ODIA", "translation": "ବେପରୁଆ ଡ୍ରାଇଭିଂ"}
            ]
        },
        {
            "key": "DRIVER_CHARGED_MORE",
            "sendPN": true,
            "priority": 4,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Driver charged more"},
                {"language": "HINDI", "translation": "ड्राइवर ने अधिक चार्ज किया"},
                {"language": "KANNADA", "translation": "ಚಾಲಕ ಹೆಚ್ಚು ಶುಲ್ಕ ವಿಧಿಸಿದನು"},
                {"language": "TAMIL", "translation": "டிரைவர் மேலும் கட்டணம் வசூலித்தார்"},
                {"language": "TELUGU", "translation": "డ్రైవర్ ఎక్కువ వసూలు చేశాడు"},
                {"language": "BENGALI", "translation": "চালকের ভাড়া বেশি"},
                {"language": "ODIA", "translation": "ଚାଳକ ଅଧିକ ଭଡ଼ା ଚାର୍ଜ କରିଛନ୍ତି"}
            ]
        },
        {
            "key": "TRIP_DELAYED",
            "sendPN": false,
            "priority": 5,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Trip delayed"},
                {"language": "HINDI", "translation": "ट्रिप में देरी हुई"},
                {"language": "KANNADA", "translation": "ಟ್ರಿಪ್ ವಿಳಂಬವಾಗಿದೆ"},
                {"language": "TAMIL", "translation": "பயணம் தாமதமானது"},
                {"language": "TELUGU", "translation": "ట్రిప్ ఆలస్యం"},
                {"language": "BENGALI", "translation": "ট্রিপ বিলম্বিত"},
                {"language": "ODIA", "translation": "ଯାତ୍ରା ବିଳମ୍ବ"}
            ]
        }
    ]'::jsonb
FROM atlas_app.merchant_operating_city moc
WHERE moc.state != 'West Bengal';

-- ============================================================================
-- DRIVER CATEGORY - Rating 1 - "Other" Section - EXCLUDING WEST BENGAL
-- ============================================================================

INSERT INTO atlas_app.feedback_form (
    id, category_name, rating, question, question_translations, answer, answer_type,
    merchant_operating_city_id, merchant_id, badges
)
SELECT
    md5(random()::text || clock_timestamp()::text)::uuid as id,
    'DRIVER', 1,
    'Other',
    '[{"language": "ENGLISH", "translation": "Other"}, {"language": "HINDI", "translation": "अन्य"}, {"language": "KANNADA", "translation": "ಇತರ"}, {"language": "TAMIL", "translation": "மற்ற"}, {"language": "TELUGU", "translation": "ఇతర"}, {"language": "BENGALI", "translation": "অন্যান্য"}, {"language": "ODIA", "translation": "ଅନ୍ୟ"}]'::json,
    ARRAY['Unclean vehicle', 'High fare']::text[],
    'Checkbox',
    moc.id, moc.merchant_id,
    '[
        {
            "key": "UNCLEAN_VEHICLE",
            "sendPN": false,
            "priority": 1,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Unclean Vehicle"},
                {"language": "HINDI", "translation": "अशुद्ध वाहन"},
                {"language": "KANNADA", "translation": "ಅಶುದ್ಧ -ವಾಹನ"},
                {"language": "TAMIL", "translation": "அசுத்த வாகனம்"},
                {"language": "TELUGU", "translation": "అపరిశుభ్రమైన వాహనం"},
                {"language": "BENGALI", "translation": "অপরিষ্কার যানবাহন"},
                {"language": "ODIA", "translation": "ଅପରିଷ୍କାର ଯାନ"}
            ]
        },
        {
            "key": "HIGH_FARE",
            "sendPN": false,
            "priority": 2,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "High Fare"},
                {"language": "HINDI", "translation": "अधिक किराया"},
                {"language": "KANNADA", "translation": "ಉನ್ನತ ಶುಲ್ಕ"},
                {"language": "TAMIL", "translation": "அதிக கட்டணம்"},
                {"language": "TELUGU", "translation": "అధిక ఛార్జీలు"},
                {"language": "BENGALI", "translation": "উচ্চ ভাড়া"},
                {"language": "ODIA", "translation": "ଉଚ୍ଚ ଭଡ଼ା"}
            ]
        }
    ]'::jsonb
FROM atlas_app.merchant_operating_city moc
WHERE moc.state != 'West Bengal';

-- ============================================================================
-- DRIVER CATEGORY - Rating 2 (Poor Experience) - EXCLUDING WEST BENGAL
-- ============================================================================

INSERT INTO atlas_app.feedback_form (
    id, category_name, rating, question, question_translations, answer, answer_type,
    merchant_operating_city_id, merchant_id, badges
)
SELECT
    md5(random()::text || clock_timestamp()::text)::uuid as id,
    'DRIVER', 2,
    'Sorry for your poor experience. Feel free to let us know your feedback about the driver.',
    '[{"language": "ENGLISH", "translation": "Sorry for your poor experience. Feel free to let us know your feedback about the driver."}, {"language": "HINDI", "translation": "आपके ख़राब अनुभव के लिए क्षमा करें। बेझिझक हमें ड्राइवर के बारे में अपनी प्रतिक्रिया बताएं।"}, {"language": "KANNADA", "translation": "ನಿಮ್ಮ ಕಳಪೆ ಅನುಭವಕ್ಕಾಗಿ ಕ್ಷಮಿಸಿ.ಚಾಲಕನ ಬಗ್ಗೆ ನಿಮ್ಮ ಪ್ರತಿಕ್ರಿಯೆಯನ್ನು ನಮಗೆ ತಿಳಿಸಲು ಹಿಂಜರಿಯಬೇಡಿ."}, {"language": "TAMIL", "translation": "உங்கள் மோசமான அனுபவத்திற்கு மன்னிக்கவும்.ஓட்டுனர் பற்றிய உங்கள் கருத்தை எங்களுக்குத் தெரியப்படுத்துங்கள்."}, {"language": "TELUGU", "translation": "మీ అసౌకర్యానికి చింతిస్తున్నాం. డ్రైవర్ గురించి మీ అభిప్రాయాన్ని మాకు తెలియజేయడానికి సంకోచించకండి."}, {"language": "BENGALI", "translation": "আপনার খারাপ অভিজ্ঞতা জন্য দুঃখিত. ড্রাইভার সম্পর্কে আপনার মতামত আমাদের জানাতে নির্দ্বিধায়।"}, {"language": "ODIA", "translation": "ଆପଣଙ୍କର ଖରାପ ଅନୁଭବ ପାଇଁ ଦୁଃଖିତ। ଡ୍ରାଇଭର ବିଷୟରେ ଆପଣଙ୍କର ପ୍ରତିକ୍ରିୟା ଦେବାକୁ ସ୍ୱାଗତ"}]'::json,
    ARRAY['Rude driver', 'Felt unsafe', 'Reckless driving', 'Driver charged more', 'Trip delayed']::text[],
    'Checkbox',
    moc.id, moc.merchant_id,
    '[
        {
            "key": "RUDE_DRIVER",
            "sendPN": true,
            "priority": 1,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Rude driver"},
                {"language": "HINDI", "translation": "असभ्य चालक"},
                {"language": "KANNADA", "translation": "ಅಸಭ್ಯ ಚಾಲಕ"},
                {"language": "TAMIL", "translation": "முரட்டுத்தனமான ஓட்டுனர்"},
                {"language": "TELUGU", "translation": "మొరటు డ్రైవర్"},
                {"language": "BENGALI", "translation": "অভদ্র ড্রাইভার"},
                {"language": "ODIA", "translation": "ଅସଭ୍ୟ ଡ୍ରାଇଭର"}
            ]
        },
        {
            "key": "FELT_UNSAFE",
            "sendPN": false,
            "priority": 2,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Felt unsafe"},
                {"language": "HINDI", "translation": "असुरक्षित लगा"},
                {"language": "KANNADA", "translation": "ಅಸುರಕ್ಷಿತ ಭಾವನೆ"},
                {"language": "TAMIL", "translation": "பாதுகாப்பற்றதாக உணர்ந்தேன்"},
                {"language": "TELUGU", "translation": "అసురక్షితంగా అనిపించింది"},
                {"language": "BENGALI", "translation": "অনিরাপদ বোধ করছেন"},
                {"language": "ODIA", "translation": "ଅସୁରକ୍ଷିତ ମନେ ହେଲା"}
            ]
        },
        {
            "key": "RECKLESS_DRIVING",
            "sendPN": false,
            "priority": 3,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Reckless driving"},
                {"language": "HINDI", "translation": "लापरवाह ड्राइविंग"},
                {"language": "KANNADA", "translation": "ಅಜಾಗರೂಕ ಚಾಲನೆ"},
                {"language": "TAMIL", "translation": "பொறுப்பற்ற வாகனம் ஓட்டுதல்"},
                {"language": "TELUGU", "translation": "నిర్లక్ష్యంగా డ్రైవింగ్"},
                {"language": "BENGALI", "translation": "বেপরোয়া গাড়ি চালানো"},
                {"language": "ODIA", "translation": "ବେପରୁଆ ଡ୍ରାଇଭିଂ"}
            ]
        },
        {
            "key": "DRIVER_CHARGED_MORE",
            "sendPN": true,
            "priority": 4,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Driver charged more"},
                {"language": "HINDI", "translation": "ड्राइवर ने अधिक चार्ज किया"},
                {"language": "KANNADA", "translation": "ಚಾಲಕ ಹೆಚ್ಚು ಶುಲ್ಕ ವಿಧಿಸಿದನು"},
                {"language": "TAMIL", "translation": "டிரைவர் மேலும் கட்டணம் வசூலித்தார்"},
                {"language": "TELUGU", "translation": "డ్రైవర్ ఎక్కువ వసూలు చేశాడు"},
                {"language": "BENGALI", "translation": "চালকের ভাড়া বেশি"},
                {"language": "ODIA", "translation": "ଚାଳକ ଅଧିକ ଭଡ଼ା ଚାର୍ଜ କରିଛନ୍ତି"}
            ]
        },
        {
            "key": "TRIP_DELAYED",
            "sendPN": false,
            "priority": 5,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Trip delayed"},
                {"language": "HINDI", "translation": "ट्रिप में देरी हुई"},
                {"language": "KANNADA", "translation": "ಟ್ರಿಪ್ ವಿಳಂಬವಾಗಿದೆ"},
                {"language": "TAMIL", "translation": "பயணம் தாமதமானது"},
                {"language": "TELUGU", "translation": "ట్రిప్ ఆలస్యం"},
                {"language": "BENGALI", "translation": "ট্রিপ বিলম্বিত"},
                {"language": "ODIA", "translation": "ଯାତ୍ରା ବିଳମ୍ବ"}
            ]
        }
    ]'::jsonb
FROM atlas_app.merchant_operating_city moc
WHERE moc.state != 'West Bengal';

-- ============================================================================
-- DRIVER CATEGORY - Rating 2 - "Other" Section - EXCLUDING WEST BENGAL
-- ============================================================================

INSERT INTO atlas_app.feedback_form (
    id, category_name, rating, question, question_translations, answer, answer_type,
    merchant_operating_city_id, merchant_id, badges
)
SELECT
    md5(random()::text || clock_timestamp()::text)::uuid as id,
    'DRIVER', 2,
    'Other',
    '[{"language": "ENGLISH", "translation": "Other"}, {"language": "HINDI", "translation": "अन्य"}, {"language": "KANNADA", "translation": "ಇತರ"}, {"language": "TAMIL", "translation": "மற்ற"}, {"language": "TELUGU", "translation": "ఇతర"}, {"language": "BENGALI", "translation": "অন্যান্য"}, {"language": "ODIA", "translation": "ଅନ୍ୟ"}]'::json,
    ARRAY['Unclean vehicle', 'High fare']::text[],
    'Checkbox',
    moc.id, moc.merchant_id,
    '[
        {
            "key": "UNCLEAN_VEHICLE",
            "sendPN": false,
            "priority": 1,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Unclean Vehicle"},
                {"language": "HINDI", "translation": "अशुद्ध वाहन"},
                {"language": "KANNADA", "translation": "ಅಶುದ್ಧ -ವಾಹನ"},
                {"language": "TAMIL", "translation": "அசுத்த வாகனம்"},
                {"language": "TELUGU", "translation": "అపరిశుభ్రమైన వాహనం"},
                {"language": "BENGALI", "translation": "অপরিষ্কার যানবাহন"},
                {"language": "ODIA", "translation": "ଅପରିଷ୍କାର ଯାନ"}
            ]
        },
        {
            "key": "HIGH_FARE",
            "sendPN": false,
            "priority": 2,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "High Fare"},
                {"language": "HINDI", "translation": "अधिक किराया"},
                {"language": "KANNADA", "translation": "ಉನ್ನತ ಶುಲ್ಕ"},
                {"language": "TAMIL", "translation": "அதிக கட்டணம்"},
                {"language": "TELUGU", "translation": "అధిక ఛార్జీలు"},
                {"language": "BENGALI", "translation": "উচ্চ ভাড়া"},
                {"language": "ODIA", "translation": "ଉଚ୍ଚ ଭଡ଼ା"}
            ]
        }
    ]'::jsonb
FROM atlas_app.merchant_operating_city moc
WHERE moc.state != 'West Bengal';

-- ============================================================================
-- DRIVER CATEGORY - Rating 3 (Average Experience) - EXCLUDING WEST BENGAL
-- ============================================================================

INSERT INTO atlas_app.feedback_form (
    id, category_name, rating, question, question_translations, answer, answer_type,
    merchant_operating_city_id, merchant_id, badges
)
SELECT
    md5(random()::text || clock_timestamp()::text)::uuid as id,
    'DRIVER', 3,
    'Sorry for your poor experience. Feel free to let us know your feedback about the driver.',
    '[{"language": "ENGLISH", "translation": "Sorry for your poor experience. Feel free to let us know your feedback about the driver."}, {"language": "HINDI", "translation": "आपके ख़राब अनुभव के लिए क्षमा करें। बेझिझक हमें ड्राइवर के बारे में अपनी प्रतिक्रिया बताएं।"}, {"language": "KANNADA", "translation": "ನಿಮ್ಮ ಕಳಪೆ ಅನುಭವಕ್ಕಾಗಿ ಕ್ಷಮಿಸಿ.ಚಾಲಕನ ಬಗ್ಗೆ ನಿಮ್ಮ ಪ್ರತಿಕ್ರಿಯೆಯನ್ನು ನಮಗೆ ತಿಳಿಸಲು ಹಿಂಜರಿಯಬೇಡಿ."}, {"language": "TAMIL", "translation": "உங்கள் மோசமான அனுபவத்திற்கு மன்னிக்கவும்.ஓட்டுனர் பற்றிய உங்கள் கருத்தை எங்களுக்குத் தெரியப்படுத்துங்கள்."}, {"language": "TELUGU", "translation": "మీ అసౌకర్యానికి చింతిస్తున్నాం. డ్రైవర్ గురించి మీ అభిప్రాయాన్ని మాకు తెలియజేయడానికి సంకోచించకండి."}, {"language": "BENGALI", "translation": "আপনার খারাপ অভিজ্ঞতা জন্য দুঃখিত. ড্রাইভার সম্পর্কে আপনার মতামত আমাদের জানাতে নির্দ্বিধায়।"}, {"language": "ODIA", "translation": "ଆପଣଙ୍କର ଖରାପ ଅନୁଭବ ପାଇଁ ଦୁଃଖିତ। ଡ୍ରାଇଭର ବିଷୟରେ ଆପଣଙ୍କର ପ୍ରତିକ୍ରିୟା ଦେବାକୁ ସ୍ୱାଗତ"}]'::json,
    ARRAY['Unprofessional driving', 'Rash driving', 'Driver charged more', 'Uncomfortable ride', 'Trip delayed', 'Felt unsafe']::text[],
    'Checkbox',
    moc.id, moc.merchant_id,
    '[
        {
            "key": "UNPROFESSIONAL_DRIVING",
            "sendPN": false,
            "priority": 1,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Unprofessional driving"},
                {"language": "HINDI", "translation": "अव्यवसायिक ड्राइविंग"},
                {"language": "KANNADA", "translation": "ವೃತ್ತಿಜೀವನದ ಚಾಲನೆ"},
                {"language": "TAMIL", "translation": "தொழில்சார் வாகனம் ஓட்டுதல்"},
                {"language": "TELUGU", "translation": "మంచి డ్రైవింగ్ కాదు"},
                {"language": "BENGALI", "translation": "অ-পেশাদার ড্রাইভিং"},
                {"language": "ODIA", "translation": "ଅପରିଶିଷ୍ଟ ଡ୍ରାଇଭିଂ"}
            ]
        },
        {
            "key": "RASH_DRIVING",
            "sendPN": true,
            "priority": 2,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Rash driving"},
                {"language": "HINDI", "translation": "लापरवाह ड्राइविंग"},
                {"language": "KANNADA", "translation": "ರಾಶ್ ಚಾಲನೆ"},
                {"language": "TAMIL", "translation": "டிரைவர் மோசமாக ஓட்டினார்"},
                {"language": "TELUGU", "translation": "రాష్ డ్రైవింగ్"},
                {"language": "BENGALI", "translation": "বেপরোয়া ড্রাইভিং"},
                {"language": "ODIA", "translation": "ରାଶ୍ ଡ୍ରାଇଭିଂ"}
            ]
        },
        {
            "key": "DRIVER_CHARGED_MORE",
            "sendPN": true,
            "priority": 3,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Driver charged more"},
                {"language": "HINDI", "translation": "ड्राइवर ने अधिक चार्ज किया"},
                {"language": "KANNADA", "translation": "ಚಾಲಕ ಹೆಚ್ಚು ಶುಲ್ಕ ವಿಧಿಸಿದನು"},
                {"language": "TAMIL", "translation": "டிரைவர் மேலும் கட்டணம் வசூலித்தார்"},
                {"language": "TELUGU", "translation": "డ్రైవర్ ఎక్కువ వసూలు చేశాడు"},
                {"language": "BENGALI", "translation": "চালকের ভাড়া বেশি"},
                {"language": "ODIA", "translation": "ଚାଳକ ଅଧିକ ଭଡ଼ା ଚାର୍ଜ କରିଛନ୍ତି"}
            ]
        },
        {
            "key": "UNCOMFORTABLE_RIDE",
            "sendPN": false,
            "priority": 4,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Uncomfortable ride"},
                {"language": "HINDI", "translation": "असहज सवारी"},
                {"language": "KANNADA", "translation": "ಅನಾನುಕೂಲ ಸವಾರಿ"},
                {"language": "TAMIL", "translation": "சங்கடமான சவாரி"},
                {"language": "TELUGU", "translation": "అసౌకర్య రైడ్"},
                {"language": "BENGALI", "translation": "অস্বস্তিকর যাত্রা"},
                {"language": "ODIA", "translation": "ଅସಹଜ ରାଇଡ୍"}
            ]
        },
        {
            "key": "TRIP_DELAYED",
            "sendPN": false,
            "priority": 5,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Trip delayed"},
                {"language": "HINDI", "translation": "ट्रिप में देरी हुई"},
                {"language": "KANNADA", "translation": "ಟ್ರಿಪ್ ವಿಳಂಬವಾಗಿದೆ"},
                {"language": "TAMIL", "translation": "பயணம் தாமதமானது"},
                {"language": "TELUGU", "translation": "ట్రిప్ ఆలస్యం"},
                {"language": "BENGALI", "translation": "ট্রিপ বিলম্বিত"},
                {"language": "ODIA", "translation": "ଯାତ୍ରା ବିଳମ୍ବ"}
            ]
        },
        {
            "key": "FELT_UNSAFE",
            "sendPN": false,
            "priority": 6,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Felt unsafe"},
                {"language": "HINDI", "translation": "असुरक्षित लगा"},
                {"language": "KANNADA", "translation": "ಅಸುರಕ್ಷಿತ ಭಾವನೆ"},
                {"language": "TAMIL", "translation": "பாதுகாப்பற்றதாக உணர்ந்தேன்"},
                {"language": "TELUGU", "translation": "అసురక్షితంగా అనిపించింది"},
                {"language": "BENGALI", "translation": "অনিরাপদ বোধ করছেন"},
                {"language": "ODIA", "translation": "ଅସୁରକ୍ଷିତ ମନେ ହେଲା"}
            ]
        }
    ]'::jsonb
FROM atlas_app.merchant_operating_city moc
WHERE moc.state != 'West Bengal';

-- ============================================================================
-- DRIVER CATEGORY - Rating 3 - "Other" Section - EXCLUDING WEST BENGAL
-- ============================================================================

INSERT INTO atlas_app.feedback_form (
    id, category_name, rating, question, question_translations, answer, answer_type,
    merchant_operating_city_id, merchant_id, badges
)
SELECT
    md5(random()::text || clock_timestamp()::text)::uuid as id,
    'DRIVER', 3,
    'Other',
    '[{"language": "ENGLISH", "translation": "Other"}, {"language": "HINDI", "translation": "अन्य"}, {"language": "KANNADA", "translation": "ಇತರ"}, {"language": "TAMIL", "translation": "மற்ற"}, {"language": "TELUGU", "translation": "ఇతர"}, {"language": "BENGALI", "translation": "অন্যান্য"}, {"language": "ODIA", "translation": "ଅନ୍ୟ"}]'::json,
    ARRAY['Unclean vehicle', 'High fare']::text[],
    'Checkbox',
    moc.id, moc.merchant_id,
    '[
        {
            "key": "UNCLEAN_VEHICLE",
            "sendPN": false,
            "priority": 1,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Unclean Vehicle"},
                {"language": "HINDI", "translation": "अशुद्ध वाहन"},
                {"language": "KANNADA", "translation": "ಅಶುದ್ಧ -ವಾಹನ"},
                {"language": "TAMIL", "translation": "அசுத்த வாகனம்"},
                {"language": "TELUGU", "translation": "అపరిశుభ్రమైన వాహనం"},
                {"language": "BENGALI", "translation": "অপরিষ্কার যানবাহন"},
                {"language": "ODIA", "translation": "ଅପରିଷ୍କାର ଯାନ"}
            ]
        },
        {
            "key": "HIGH_FARE",
            "sendPN": false,
            "priority": 2,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "High Fare"},
                {"language": "HINDI", "translation": "अधिक किराया"},
                {"language": "KANNADA", "translation": "ಉನ್ನತ ಶುಲ್ಕ"},
                {"language": "TAMIL", "translation": "அதிக கட்டணம்"},
                {"language": "TELUGU", "translation": "అధిక ఛార్జీలు"},
                {"language": "BENGALI", "translation": "উচ্চ ভাড়া"},
                {"language": "ODIA", "translation": "ଉଚ୍ଚ ଭଡ଼ା"}
            ]
        }
    ]'::jsonb
FROM atlas_app.merchant_operating_city moc
WHERE moc.state != 'West Bengal';

-- ============================================================================
-- DRIVER CATEGORY - Rating 4 (Good Experience) - EXCLUDING WEST BENGAL
-- ============================================================================

INSERT INTO atlas_app.feedback_form (
    id, category_name, rating, question, question_translations, answer, answer_type,
    merchant_operating_city_id, merchant_id, badges
)
SELECT
    md5(random()::text || clock_timestamp()::text)::uuid as id,
    'DRIVER', 4,
    'Driver related feedback',
    '[{"language": "ENGLISH", "translation": "Driver related feedback"}, {"language": "HINDI", "translation": "ड्राइवर से संबंधित प्रतिक्रिया"}, {"language": "KANNADA", "translation": "ಚಾಲಕ ಸಂಬಂಧಿತ ಪ್ರತಿಕ್ರಿಯೆ"}, {"language": "TAMIL", "translation": "ஓட்டுனர் தொடர்பான கருத்து"}, {"language": "TELUGU", "translation": "డ్రైవర్ సంబంధిత అభిప్రాయం"}, {"language": "BENGALI", "translation": "ড্রাইভার সম্পর্কিত প্রতিক্রিয়া"}, {"language": "ODIA", "translation": "ଡ୍ରାଇଭର ସମ୍ବନ୍ଧୀୟ ମତାମତ"}]'::json,
    ARRAY['Polite driver', 'Expert driving', 'Asked for extra fare', 'Safe ride', 'Uncomfortable ride', 'Trip got delayed']::text[],
    'Checkbox',
    moc.id, moc.merchant_id,
    '[
        {
            "key": "POLITE_DRIVER",
            "sendPN": false,
            "priority": 1,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Polite driver"},
                {"language": "HINDI", "translation": "विनम्र चालक"},
                {"language": "KANNADA", "translation": "ಸಭ್ಯ ಚಾಲಕ"},
                {"language": "TAMIL", "translation": "பணிவான டிரைவர்"},
                {"language": "TELUGU", "translation": "మర్యాదపూర్వక డ్రైవర్"},
                {"language": "BENGALI", "translation": "ভদ্র ড্রাইভার"},
                {"language": "ODIA", "translation": "ଭଦ୍ର ଡ୍ରାଇଭର"}
            ]
        },
        {
            "key": "EXPERT_DRIVING",
            "sendPN": false,
            "priority": 2,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Expert driving"},
                {"language": "HINDI", "translation": "कुशल ड्राइविंग"},
                {"language": "KANNADA", "translation": "ಪರಿಣಿತ ಚಾಲನೆ"},
                {"language": "TAMIL", "translation": "டிரைவர் நன்றாக ஓட்டினார்"},
                {"language": "TELUGU", "translation": "నిపుణమైన డ్రైవింగ్"},
                {"language": "BENGALI", "translation": "এক্সপার্ট ড্রাইভিং"},
                {"language": "ODIA", "translation": "ବିଶେଷଜ୍ଞ ଡ୍ରାଇଭିଂ"}
            ]
        },
        {
            "key": "ASKED_FOR_EXTRA_FARE",
            "sendPN": false,
            "priority": 3,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Asked for extra fare"},
                {"language": "HINDI", "translation": "अतिरिक्त किराया मांगा"},
                {"language": "KANNADA", "translation": "ಹೆಚ್ಚುವರಿ ಶುಲ್ಕವನ್ನು ಕೇಳಿದೆ"},
                {"language": "TAMIL", "translation": "கூடுதல் கட்டணம் கேட்டார்கள்"},
                {"language": "TELUGU", "translation": "అదనపు ఛార్జీలు అడిగారు"},
                {"language": "BENGALI", "translation": "বাড়তি ভাড়া চেয়েছেন"},
                {"language": "ODIA", "translation": "ଅତିରିକ୍ତ ଭଡ଼ା ମାଗିଲେ"}
            ]
        },
        {
            "key": "SAFE_RIDE",
            "sendPN": false,
            "priority": 4,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Safe ride"},
                {"language": "HINDI", "translation": "सुरक्षित सवारी"},
                {"language": "KANNADA", "translation": "ಸುರಕ್ಷಿತ ಸವಾರಿ"},
                {"language": "TAMIL", "translation": "பாதுகாப்பான சவாரி"},
                {"language": "TELUGU", "translation": "సురక్షిత రైడ్"},
                {"language": "BENGALI", "translation": "নিরাপদ যাত্রা"},
                {"language": "ODIA", "translation": "ସୁରକ୍ଷିତ ରାଇଡ୍"}
            ]
        },
        {
            "key": "UNCOMFORTABLE_RIDE",
            "sendPN": false,
            "priority": 5,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Uncomfortable ride"},
                {"language": "HINDI", "translation": "असहज सवारी"},
                {"language": "KANNADA", "translation": "ಅನಾನುಕೂಲ ಸವಾರಿ"},
                {"language": "TAMIL", "translation": "சங்கடமான சவாரி"},
                {"language": "TELUGU", "translation": "అసౌకర్య రైడ్"},
                {"language": "BENGALI", "translation": "অস্বস্তিকর যাত্রা"},
                {"language": "ODIA", "translation": "ଅସହଜ ରାଇଡ୍"}
            ]
        },
        {
            "key": "TRIP_GOT_DELAYED",
            "sendPN": false,
            "priority": 6,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Trip got delayed"},
                {"language": "HINDI", "translation": "यात्रा में देरी हुई"},
                {"language": "KANNADA", "translation": "ಟ್ರಿಪ್ ವಿಳಂಬವಾಯಿತು"},
                {"language": "TAMIL", "translation": "பயணம் தாமதமானது"},
                {"language": "TELUGU", "translation": "ట్రిప్ ఆలస్యం అయ్యింది"},
                {"language": "BENGALI", "translation": "যাত্রা বিলম্বিত হয়েছে"},
                {"language": "ODIA", "translation": "ଯାତ୍ରା ବିଳମ୍ବ ହେଲା"}
            ]
        }
    ]'::jsonb
FROM atlas_app.merchant_operating_city moc
WHERE moc.state != 'West Bengal';

-- ============================================================================
-- DRIVER CATEGORY - Rating 4 - "Other" Section - EXCLUDING WEST BENGAL
-- ============================================================================

INSERT INTO atlas_app.feedback_form (
    id, category_name, rating, question, question_translations, answer, answer_type,
    merchant_operating_city_id, merchant_id, badges
)
SELECT
    md5(random()::text || clock_timestamp()::text)::uuid as id,
    'DRIVER', 4,
    'Other',
    '[{"language": "ENGLISH", "translation": "Other"}, {"language": "HINDI", "translation": "अन्य"}, {"language": "KANNADA", "translation": "ಇತರ"}, {"language": "TAMIL", "translation": "மற்ற"}, {"language": "TELUGU", "translation": "ఇతర"}, {"language": "BENGALI", "translation": "অন্যান্য"}, {"language": "ODIA", "translation": "ଅନ୍ୟ"}]'::json,
    ARRAY['Clean vehicle', 'Right fare']::text[],
    'Checkbox',
    moc.id, moc.merchant_id,
    '[
        {
            "key": "CLEAN_VEHICLE",
            "sendPN": false,
            "priority": 1,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Clean vehicle"},
                {"language": "HINDI", "translation": "स्वच्छ वाहन"},
                {"language": "KANNADA", "translation": "ಶುದ್ಧ ವಾಹನ"},
                {"language": "TAMIL", "translation": "சுத்தமான வாகனம்"},
                {"language": "TELUGU", "translation": "శుభ్రమైన వాహనం"},
                {"language": "BENGALI", "translation": "পরিচ্ছন্ন যানবাহন"},
                {"language": "ODIA", "translation": "ପରିଷ୍କାର ଯାନ"}
            ]
        },
        {
            "key": "RIGHT_FARE",
            "sendPN": false,
            "priority": 2,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Right Fare"},
                {"language": "HINDI", "translation": "सही किराया"},
                {"language": "KANNADA", "translation": "ಸರಿಯಾದ ದರ"},
                {"language": "TAMIL", "translation": "சரியான கட்டணம்"},
                {"language": "TELUGU", "translation": "సరైన ఛార్జీలు"},
                {"language": "BENGALI", "translation": "সঠিক ভাড়া"},
                {"language": "ODIA", "translation": "ଠିକ୍ ଭଡ଼ା"}
            ]
        }
    ]'::jsonb
FROM atlas_app.merchant_operating_city moc
WHERE moc.state != 'West Bengal';

-- ============================================================================
-- DRIVER CATEGORY - Rating 5 (Excellent Experience) - EXCLUDING WEST BENGAL
-- ============================================================================

INSERT INTO atlas_app.feedback_form (
    id, category_name, rating, question, question_translations, answer, answer_type,
    merchant_operating_city_id, merchant_id, badges
)
SELECT
    md5(random()::text || clock_timestamp()::text)::uuid as id,
    'DRIVER', 5,
    'Driver related feedback',
    '[{"language": "ENGLISH", "translation": "Driver related feedback"}, {"language": "HINDI", "translation": "ड्राइवर से संबंधित प्रतिक्रिया"}, {"language": "KANNADA", "translation": "ಚಾಲಕ ಸಂಬಂಧಿತ ಪ್ರತಿಕ್ರಿಯೆ"}, {"language": "TAMIL", "translation": "ஓட்டுனர் தொடர்பான கருத்து"}, {"language": "TELUGU", "translation": "డ్రైవర్ సంబంధిత అభిప్రాయం"}, {"language": "BENGALI", "translation": "ড্রাইভার সম্পর্কিত প্রতিক্রিয়া"}, {"language": "ODIA", "translation": "ଡ୍ରାଇଭର ସମ୍ବନ୍ଧୀୟ ମତାମତ"}]'::json,
    ARRAY['Polite driver', 'Expert driving', 'On time', 'Skilled navigator', 'Safe ride']::text[],
    'Checkbox',
    moc.id, moc.merchant_id,
    '[
        {
            "key": "POLITE_DRIVER",
            "sendPN": false,
            "priority": 1,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Polite driver"},
                {"language": "HINDI", "translation": "विनम्र चालक"},
                {"language": "KANNADA", "translation": "ಸಭ್ಯ ಚಾಲಕ"},
                {"language": "TAMIL", "translation": "பணிவான டிரைவர்"},
                {"language": "TELUGU", "translation": "మర్యాదపూర్వక డ్రైవర్"},
                {"language": "BENGALI", "translation": "ভদ্র ড্রাইভার"},
                {"language": "ODIA", "translation": "ଭଦ୍ର ଡ୍ରାଇଭର"}
            ]
        },
        {
            "key": "EXPERT_DRIVING",
            "sendPN": false,
            "priority": 2,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Expert driving"},
                {"language": "HINDI", "translation": "कुशल ड्राइविंग"},
                {"language": "KANNADA", "translation": "ಪರಿಣಿತ ಚಾಲನೆ"},
                {"language": "TAMIL", "translation": "டிரைவர் நன்றாக ஓட்டினார்"},
                {"language": "TELUGU", "translation": "నిపుణమైన డ్రైవింగ్"},
                {"language": "BENGALI", "translation": "এক্সপার্ট ড্রাইভিং"},
                {"language": "ODIA", "translation": "ବିଶେଷଜ୍ଞ ଡ୍ରାଇଭିଂ"}
            ]
        },
        {
            "key": "ON_TIME",
            "sendPN": false,
            "priority": 3,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "On time"},
                {"language": "HINDI", "translation": "समय पर"},
                {"language": "KANNADA", "translation": "ಸಮಯಕ್ಕೆ"},
                {"language": "TAMIL", "translation": "சரியான நேரத்தில்"},
                {"language": "TELUGU", "translation": "సమయానికి"},
                {"language": "BENGALI", "translation": "সময়মতো"},
                {"language": "ODIA", "translation": "ସମୟରେ"}
            ]
        },
        {
            "key": "SKILLED_NAVIGATOR",
            "sendPN": false,
            "priority": 4,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Skilled navigator"},
                {"language": "HINDI", "translation": "कुशल नाविक"},
                {"language": "KANNADA", "translation": "ನುರಿತ ನ್ಯಾವಿಗೇಟರ್"},
                {"language": "TAMIL", "translation": "திறமையான நேவிகேட்டர்"},
                {"language": "TELUGU", "translation": "నైపుణ్యం కలిగిన నావిగేటర్"},
                {"language": "BENGALI", "translation": "দক্ষ নেভিগেটর"},
                {"language": "ODIA", "translation": "ଦକ୍ଷ ନାଭିଗେଟର୍"}
            ]
        },
        {
            "key": "SAFE_RIDE",
            "sendPN": false,
            "priority": 5,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Safe ride"},
                {"language": "HINDI", "translation": "सुरक्षित सवारी"},
                {"language": "KANNADA", "translation": "ಸುರಕ್ಷಿತ ಸವಾರಿ"},
                {"language": "TAMIL", "translation": "பாதுகாப்பான சவாரி"},
                {"language": "TELUGU", "translation": "సురక్షిత రైడ్"},
                {"language": "BENGALI", "translation": "নিরাপদ যাত্রা"},
                {"language": "ODIA", "translation": "ସୁରକ୍ଷିତ ରାଇଡ୍"}
            ]
        }
    ]'::jsonb
FROM atlas_app.merchant_operating_city moc
WHERE moc.state != 'West Bengal';

-- ============================================================================
-- DRIVER CATEGORY - Rating 5 - "Other" Section - EXCLUDING WEST BENGAL
-- ============================================================================

INSERT INTO atlas_app.feedback_form (
    id, category_name, rating, question, question_translations, answer, answer_type,
    merchant_operating_city_id, merchant_id, badges
)
SELECT
    md5(random()::text || clock_timestamp()::text)::uuid as id,
    'DRIVER', 5,
    'Other',
    '[{"language": "ENGLISH", "translation": "Other"}, {"language": "HINDI", "translation": "अन्य"}, {"language": "KANNADA", "translation": "ಇತರ"}, {"language": "TAMIL", "translation": "மற்ற"}, {"language": "TELUGU", "translation": "ఇతర"}, {"language": "BENGALI", "translation": "অন্যান্য"}, {"language": "ODIA", "translation": "ଅନ୍ୟ"}]'::json,
    ARRAY['Clean vehicle', 'Right fare']::text[],
    'Checkbox',
    moc.id, moc.merchant_id,
    '[
        {
            "key": "CLEAN_VEHICLE",
            "sendPN": false,
            "priority": 1,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Clean vehicle"},
                {"language": "HINDI", "translation": "स्वच्छ वाहन"},
                {"language": "KANNADA", "translation": "ಶುದ್ಧ ವಾಹನ"},
                {"language": "TAMIL", "translation": "சுத்தமான வாகனம்"},
                {"language": "TELUGU", "translation": "శుభ్రమైన వాహనం"},
                {"language": "BENGALI", "translation": "পরিচ্ছন্ন যানবাহন"},
                {"language": "ODIA", "translation": "ପରିଷ୍କାର ଯାନ"}
            ]
        },
        {
            "key": "RIGHT_FARE",
            "sendPN": false,
            "priority": 2,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Right Fare"},
                {"language": "HINDI", "translation": "सही किराया"},
                {"language": "KANNADA", "translation": "ಸರಿಯಾದ ದರ"},
                {"language": "TAMIL", "translation": "சரியான கட்டணம்"},
                {"language": "TELUGU", "translation": "సరైన ఛార్జీలు"},
                {"language": "BENGALI", "translation": "সঠিক ভাড়া"},
                {"language": "ODIA", "translation": "ଠିକ୍ ଭଡ଼ା"}
            ]
        }
    ]'::jsonb
FROM atlas_app.merchant_operating_city moc
WHERE moc.state != 'West Bengal';

-- ============================================================================
-- WEST BENGAL SPECIFIC FORMS
-- ============================================================================
-- These forms apply only to West Bengal state with different badge sets
-- Ratings 1, 2, 3, 4, 5 have West Bengal-specific badges
-- ============================================================================

-- ============================================================================
-- DRIVER CATEGORY - Rating 1 (Very Poor Experience) - WEST BENGAL ONLY
-- ============================================================================

INSERT INTO atlas_app.feedback_form (
    id, category_name, rating, question, question_translations, answer, answer_type,
    merchant_operating_city_id, merchant_id, badges
)
SELECT
    md5(random()::text || clock_timestamp()::text)::uuid as id,
    'DRIVER', 1,
    'Sorry for your poor experience. Feel free to let us know your feedback about the driver.',
    '[{"language": "ENGLISH", "translation": "Sorry for your poor experience. Feel free to let us know your feedback about the driver."}, {"language": "HINDI", "translation": "आपके ख़राब अनुभव के लिए क्षमा करें। बेझिझक हमें ड्राइवर के बारे में अपनी प्रतिक्रिया बताएं।"}, {"language": "KANNADA", "translation": "ನಿಮ್ಮ ಕಳಪೆ ಅನುಭವಕ್ಕಾಗಿ ಕ್ಷಮಿಸಿ.ಚಾಲಕನ ಬಗ್ಗೆ ನಿಮ್ಮ ಪ್ರತಿಕ್ರಿಯೆಯನ್ನು ನಮಗೆ ತಿಳಿಸಲು ಹಿಂಜರಿಯಬೇಡಿ."}, {"language": "TAMIL", "translation": "உங்கள் மோசமான அனுபவத்திற்கு மன்னிக்கவும்.ஓட்டுனர் பற்றிய உங்கள் கருத்தை எங்களுக்குத் தெரியப்படுத்துங்கள்."}, {"language": "TELUGU", "translation": "మీ అసౌకర్యానికి చింతిస్తున్నాం. డ్రైవర్ గురించి మీ అభిప్రాయాన్ని మాకు తెలియజేయడానికి సంకోచించకండి."}, {"language": "BENGALI", "translation": "আপনার খারাপ অভিজ্ঞতা জন্য দুঃখিত. ড্রাইভার সম্পর্কে আপনার মতামত আমাদের জানাতে নির্দ্বিধায়।"}, {"language": "ODIA", "translation": "ଆପଣଙ୍କର ଖରାପ ଅନୁଭବ ପାଇଁ ଦୁଃଖିତ। ଡ୍ରାଇଭର ବିଷୟରେ ଆପଣଙ୍କର ପ୍ରତିକ୍ରିୟା ଦେବାକୁ ସ୍ୱାଗତ"}]'::json,
    ARRAY['Rude driver', 'Felt unsafe', 'Reckless driving', 'Driver charged more', 'AC was not ON', 'Bad/Unclean Vehicle', 'App issues', 'Route not Followed']::text[],
    'Checkbox',
    moc.id, moc.merchant_id,
    '[
        {
            "key": "RUDE_DRIVER",
            "sendPN": true,
            "priority": 1,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Rude driver"},
                {"language": "HINDI", "translation": "असभ्य चालक"},
                {"language": "KANNADA", "translation": "ಅಸಭ್ಯ ಚಾಲಕ"},
                {"language": "TAMIL", "translation": "முரட்டுத்தனமான ஓட்டுனர்"},
                {"language": "TELUGU", "translation": "మొరటు డ్రైవర్"},
                {"language": "BENGALI", "translation": "অভদ্র ড্রাইভার"},
                {"language": "ODIA", "translation": "ଅସଭ୍ୟ ଡ୍ରାଇଭର"}
            ]
        },
        {
            "key": "FELT_UNSAFE",
            "sendPN": false,
            "priority": 2,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Felt unsafe"},
                {"language": "HINDI", "translation": "असुरक्षित लगा"},
                {"language": "KANNADA", "translation": "ಅಸುರಕ್ಷಿತ ಭಾವನೆ"},
                {"language": "TAMIL", "translation": "பாதுகாப்பற்றதாக உணர்ந்தேன்"},
                {"language": "TELUGU", "translation": "అసురక్షితంగా అనిపించింది"},
                {"language": "BENGALI", "translation": "অনিরাপদ বোধ করছেন"},
                {"language": "ODIA", "translation": "ଅସୁରକ୍ଷିତ ମନେ ହେଲା"}
            ]
        },
        {
            "key": "RECKLESS_DRIVING",
            "sendPN": false,
            "priority": 3,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Reckless driving"},
                {"language": "HINDI", "translation": "लापरवाह ड्राइविंग"},
                {"language": "KANNADA", "translation": "ಅಜಾಗರೂಕ ಚಾಲನೆ"},
                {"language": "TAMIL", "translation": "பொறுப்பற்ற வாகனம் ஓட்டுதல்"},
                {"language": "TELUGU", "translation": "నిర్లక్ష్యంగా డ్రైविंिंग"},
                {"language": "BENGALI", "translation": "বেপরোয়া গাড়ি চালানো"},
                {"language": "ODIA", "translation": "ବେପରୁଆ ଡ୍ରାଇଭିଂ"}
            ]
        },
        {
            "key": "DRIVER_CHARGED_MORE",
            "sendPN": true,
            "priority": 4,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Driver charged more"},
                {"language": "HINDI", "translation": "ड्राइवर ने अधिक चार्ज किया"},
                {"language": "KANNADA", "translation": "ಚಾಲಕ ಹೆಚ್ಚು ಶುಲ್ಕ ವಿಧಿಸಿದನು"},
                {"language": "TAMIL", "translation": "டிரைவர் மேலும் கட்டணம் வசூலித்தார்"},
                {"language": "TELUGU", "translation": "డ్రైవర్ ఎక్కువ వసూలు చేశాడు"},
                {"language": "BENGALI", "translation": "চালকের ভাড়া বেশি"},
                {"language": "ODIA", "translation": "ଚାଳକ ଅଧିକ ଭଡ଼ା ଚାର୍ଜ କରିଛନ୍ତି"}
            ]
        },
        {
            "key": "AC_WAS_NOT_ON",
            "sendPN": false,
            "priority": 5,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "AC was not ON"},
                {"language": "HINDI", "translation": "एसी चालू नहीं था"},
                {"language": "KANNADA", "translation": "AC ಆನ್ ಆಗಿರಲಿಲ್ಲ"},
                {"language": "TAMIL", "translation": "AC இயக்கத்தில் இல்லை"},
                {"language": "TELUGU", "translation": "AC ఆన్ కాలేదు"},
                {"language": "BENGALI", "translation": "এসি চালু ছিল না"},
                {"language": "ODIA", "translation": "AC ଚାଲୁ ନଥିଲା"}
            ]
        },
        {
            "key": "BAD_UNCLEAN_VEHICLE",
            "sendPN": false,
            "priority": 6,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Bad/Unclean Vehicle"},
                {"language": "HINDI", "translation": "खराब/गंदा वाहन"},
                {"language": "KANNADA", "translation": "ಕೆಟ್ಟ/ಅಶುದ್ಧ ವಾಹನ"},
                {"language": "TAMIL", "translation": "மோசமான/அசுத்த வாகனம்"},
                {"language": "TELUGU", "translation": "చెడు/అపరిశుభ్రమైన వాహనం"},
                {"language": "BENGALI", "translation": "খারাপ/অপরিষ্কার যানবাহন"},
                {"language": "ODIA", "translation": "ଖରାପ/ଅପରିଷ୍କାର ଯାନ"}
            ]
        },
        {
            "key": "APP_ISSUES",
            "sendPN": false,
            "priority": 7,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "App issues"},
                {"language": "HINDI", "translation": "ऐप की समस्याएं"},
                {"language": "KANNADA", "translation": "ಆಪ್ ಸಮಸ್ಯೆಗಳು"},
                {"language": "TAMIL", "translation": "ஆப் சிக்கல்கள்"},
                {"language": "TELUGU", "translation": "యాప్ సమస్యలు"},
                {"language": "BENGALI", "translation": "অ্যাপ সমস্যা"},
                {"language": "ODIA", "translation": "ଆପ୍ ସମସ୍ୟା"}
            ]
        },
        {
            "key": "ROUTE_NOT_FOLLOWED",
            "sendPN": false,
            "priority": 8,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Route not Followed"},
                {"language": "HINDI", "translation": "रास्ता नहीं अपनाया"},
                {"language": "KANNADA", "translation": "ಮಾರ್ಗವನ್ನು ಅನುಸರಿಸಲಿಲ್ಲ"},
                {"language": "TAMIL", "translation": "வழி பின்பற்றப்படவில்லை"},
                {"language": "TELUGU", "translation": "మార్గం అనుసరించలేదు"},
                {"language": "BENGALI", "translation": "রুট অনুসরণ করেনি"},
                {"language": "ODIA", "translation": "ମାର୍ଗ ଅନୁସରଣ କରାଯାଇ ନାହିଁ"}
            ]
        }
    ]'::jsonb
FROM atlas_app.merchant_operating_city moc
WHERE moc.state = 'West Bengal';


-- ============================================================================
-- DRIVER CATEGORY - Rating 2 (Poor Experience) - WEST BENGAL ONLY
-- ============================================================================

INSERT INTO atlas_app.feedback_form (
    id, category_name, rating, question, question_translations, answer, answer_type,
    merchant_operating_city_id, merchant_id, badges
)
SELECT
    md5(random()::text || clock_timestamp()::text)::uuid as id,
    'DRIVER', 2,
    'Sorry for your poor experience. Feel free to let us know your feedback about the driver.',
    '[{"language": "ENGLISH", "translation": "Sorry for your poor experience. Feel free to let us know your feedback about the driver."}, {"language": "HINDI", "translation": "आपके ख़राब अनुभव के लिए क्षमा करें। बेझिझक हमें ड्राइवर के बारे में अपनी प्रतिक्रिया बताएं।"}, {"language": "KANNADA", "translation": "ನಿಮ್ಮ ಕಳಪೆ ಅನುಭವಕ್ಕಾಗಿ ಕ್ಷಮಿಸಿ.ಚಾಲಕನ ಬಗ್ಗೆ ನಿಮ್ಮ ಪ್ರತಿಕ್ರಿಯೆಯನ್ನು ನಮಗೆ ತಿಳಿಸಲು ಹಿಂಜರಿಯಬೇಡಿ."}, {"language": "TAMIL", "translation": "உங்கள் மோசமான அனுபவத்திற்கு மன்னிக்கவும்.ஓட்டுனர் பற்றிய உங்கள் கருத்தை எங்களுக்குத் தெரியப்படுத்துங்கள்."}, {"language": "TELUGU", "translation": "మీ అసౌకర్యానికి చింతిస్తున్నాం. డ్రైవర్ గురించి మీ అభిప్రాయాన్ని మాకు తెలియజేయడానికి సంకోచించకండి."}, {"language": "BENGALI", "translation": "আপনার খারাপ অভিজ্ঞতা জন্য দুঃখিত. ড্রাইভার সম্পর্কে আপনার মতামত আমাদের জানাতে নির্দ্বিধায়।"}, {"language": "ODIA", "translation": "ଆପଣଙ୍କର ଖରାପ ଅନୁଭବ ପାଇଁ ଦୁଃଖିତ। ଡ୍ରାଇଭର ବିଷୟରେ ଆପଣଙ୍କର ପ୍ରତିକ୍ରିୟା ଦେବାକୁ ସ୍ୱାଗତ"}]'::json,
    ARRAY['Rude driver', 'Felt unsafe', 'Reckless driving', 'Driver charged more', 'AC was not ON', 'Bad/Unclean Vehicle', 'App issues', 'Route not Followed']::text[],
    'Checkbox',
    moc.id, moc.merchant_id,
    '[
        {
            "key": "RUDE_DRIVER",
            "sendPN": true,
            "priority": 1,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Rude driver"},
                {"language": "HINDI", "translation": "असभ्य चालक"},
                {"language": "KANNADA", "translation": "ಅಸಭ್ಯ ಚಾಲಕ"},
                {"language": "TAMIL", "translation": "முரட்டுத்தனமான ஓட்டுனர்"},
                {"language": "TELUGU", "translation": "మొరటు డ్రైవర్"},
                {"language": "BENGALI", "translation": "অভদ্র ড্রাইভার"},
                {"language": "ODIA", "translation": "ଅସଭ୍ୟ ଡ୍ରାଇଭର"}
            ]
        },
        {
            "key": "FELT_UNSAFE",
            "sendPN": false,
            "priority": 2,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Felt unsafe"},
                {"language": "HINDI", "translation": "असुरक्षित लगा"},
                {"language": "KANNADA", "translation": "ಅಸುರಕ್ಷಿತ ಭಾವನೆ"},
                {"language": "TAMIL", "translation": "பாதுகாப்பற்றதாக உணர்ந்தேன்"},
                {"language": "TELUGU", "translation": "అసురక్షితంగా అనిపించింది"},
                {"language": "BENGALI", "translation": "অনিরাপদ বোধ করছেন"},
                {"language": "ODIA", "translation": "ଅସୁରକ୍ଷିତ ମନେ ହେଲା"}
            ]
        },
        {
            "key": "RECKLESS_DRIVING",
            "sendPN": false,
            "priority": 3,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Reckless driving"},
                {"language": "HINDI", "translation": "लापरवाह ड्राइविंग"},
                {"language": "KANNADA", "translation": "ಅಜಾಗರೂಕ ಚಾಲನೆ"},
                {"language": "TAMIL", "translation": "பொறுப்பற்ற வாகனம் ஓட்டுதல்"},
                {"language": "TELUGU", "translation": "నిర్లక్ష్యంగా డ్రైವింగ్"},
                {"language": "BENGALI", "translation": "বেপরোয়া গাড়ি চালানো"},
                {"language": "ODIA", "translation": "ବେପରୁଆ ଡ୍ରାଇଭିଂ"}
            ]
        },
        {
            "key": "DRIVER_CHARGED_MORE",
            "sendPN": true,
            "priority": 4,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Driver charged more"},
                {"language": "HINDI", "translation": "ड्राइवर ने अधिक चार्ज किया"},
                {"language": "KANNADA", "translation": "ಚಾಲಕ ಹೆಚ್ಚು ಶುಲ್ಕ ವಿಧಿಸಿದನು"},
                {"language": "TAMIL", "translation": "டிரைவர் மேலும் கட்டணம் வசூலித்தார்"},
                {"language": "TELUGU", "translation": "డ్రైవర్ ఎక్కువ వసూలు చేశాడు"},
                {"language": "BENGALI", "translation": "চালকের ভাড়া বেশি"},
                {"language": "ODIA", "translation": "ଚାଳକ ଅଧିକ ଭଡ଼ା ଚାର୍ଜ କରିଛନ୍ତି"}
            ]
        },
        {
            "key": "AC_WAS_NOT_ON",
            "sendPN": false,
            "priority": 5,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "AC was not ON"},
                {"language": "HINDI", "translation": "एसी चालू नहीं था"},
                {"language": "KANNADA", "translation": "AC ಆನ್ ಆಗಿರಲಿಲ್ಲ"},
                {"language": "TAMIL", "translation": "AC இயக்கத்தில் இல்லை"},
                {"language": "TELUGU", "translation": "AC ఆన్ కాలేదు"},
                {"language": "BENGALI", "translation": "এসি চালু ছিল না"},
                {"language": "ODIA", "translation": "AC ଚାଲୁ ନଥିଲା"}
            ]
        },
        {
            "key": "BAD_UNCLEAN_VEHICLE",
            "sendPN": false,
            "priority": 6,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Bad/Unclean Vehicle"},
                {"language": "HINDI", "translation": "खराब/गंदा वाहन"},
                {"language": "KANNADA", "translation": "ಕೆಟ್ಟ/ಅಶುದ್ಧ ವಾಹನ"},
                {"language": "TAMIL", "translation": "மோசமான/அசுத்த வாகனம்"},
                {"language": "TELUGU", "translation": "చెడు/అపరిశుభ్రమైన వాహనం"},
                {"language": "BENGALI", "translation": "খারাপ/অপরিষ্কার যানবাহন"},
                {"language": "ODIA", "translation": "ଖରାପ/ଅପରିଷ୍କାର ଯାନ"}
            ]
        },
        {
            "key": "APP_ISSUES",
            "sendPN": false,
            "priority": 7,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "App issues"},
                {"language": "HINDI", "translation": "ऐप की समस्याएं"},
                {"language": "KANNADA", "translation": "ಆಪ್ ಸಮಸ್ಯೆಗಳು"},
                {"language": "TAMIL", "translation": "ஆப் சிக்கல்கள்"},
                {"language": "TELUGU", "translation": "యాప్ సమస్యలు"},
                {"language": "BENGALI", "translation": "অ্যাপ সমস্যা"},
                {"language": "ODIA", "translation": "ଆପ୍ ସମସ୍ୟା"}
            ]
        },
        {
            "key": "ROUTE_NOT_FOLLOWED",
            "sendPN": false,
            "priority": 8,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Route not Followed"},
                {"language": "HINDI", "translation": "रास्ता नहीं अपनाया"},
                {"language": "KANNADA", "translation": "ಮಾರ್ಗವನ್ನು ಅನುಸರಿಸಲಿಲ್ಲ"},
                {"language": "TAMIL", "translation": "வழி பின்பற்றப்படவில்லை"},
                {"language": "TELUGU", "translation": "మార్గం అనుసరించలేదు"},
                {"language": "BENGALI", "translation": "রুট অনুসরণ করেনি"},
                {"language": "ODIA", "translation": "ମାର୍ଗ ଅନୁସରଣ କରାଯାଇ ନାହିଁ"}
            ]
        }
    ]'::jsonb
FROM atlas_app.merchant_operating_city moc
WHERE moc.state = 'West Bengal';


-- ============================================================================
-- DRIVER CATEGORY - Rating 3 (Average Experience) - WEST BENGAL ONLY
-- ============================================================================

INSERT INTO atlas_app.feedback_form (
    id, category_name, rating, question, question_translations, answer, answer_type,
    merchant_operating_city_id, merchant_id, badges
)
SELECT
    md5(random()::text || clock_timestamp()::text)::uuid as id,
    'DRIVER', 3,
    'Sorry for your poor experience. Feel free to let us know your feedback about the driver.',
    '[{"language": "ENGLISH", "translation": "Sorry for your poor experience. Feel free to let us know your feedback about the driver."}, {"language": "HINDI", "translation": "आपके ख़राब अनुभव के लिए क्षमा करें। बेझिझक हमें ड्राइवर के बारे में अपनी प्रतिक्रिया बताएं।"}, {"language": "KANNADA", "translation": "ನಿಮ್ಮ ಕಳಪೆ ಅನುಭವಕ್ಕಾಗಿ ಕ್ಷಮಿಸಿ.ಚಾಲಕನ ಬಗ್ಗೆ ನಿಮ್ಮ ಪ್ರತಿಕ್ರಿಯೆಯನ್ನು ನಮಗೆ ತಿಳಿಸಲು ಹಿಂಜರಿಯಬೇಡಿ."}, {"language": "TAMIL", "translation": "உங்கள் மோசமான அனுபவத்திற்கு மன்னிக்கவும்.ஓட்டுனர் பற்றிய உங்கள் கருத்தை எங்களுக்குத் தெரியப்படுத்துங்கள்."}, {"language": "TELUGU", "translation": "మీ అసౌకర్యానికి చింతిస్తున్నాం. డ్రైవర్ గురించి మీ అభిప్రాయాన్ని మాకు తెలియజేయడానికి సంకోచించకండి."}, {"language": "BENGALI", "translation": "আপনার খারাপ অভিজ্ঞতা জন্য দুঃখিত. ড্রাইভার সম্পর্কে আপনার মতামত আমাদের জানাতে নির্দ্বিধায়।"}, {"language": "ODIA", "translation": "ଆପଣଙ୍କର ଖରାପ ଅନୁଭବ ପାଇଁ ଦୁଃଖିତ। ଡ୍ରାଇଭର ବିଷୟରେ ଆପଣଙ୍କର ପ୍ରତିକ୍ରିୟା ଦେବାକୁ ସ୍ୱାଗତ"}]'::json,
    ARRAY['Unprofessional driving', 'Asked for Extra', 'Rude Driver', 'Route not Followed', 'App issues', 'Felt Unsafe', 'Unclean Vehicle', 'AC not cooling']::text[],
    'Checkbox',
    moc.id, moc.merchant_id,
    '[
        {
            "key": "UNPROFESSIONAL_DRIVING",
            "sendPN": false,
            "priority": 1,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Unprofessional driving"},
                {"language": "HINDI", "translation": "अव्यवसायिक ड्राइविंग"},
                {"language": "KANNADA", "translation": "ವೃತ್ತಿಜೀವನದ ಚಾಲನೆ"},
                {"language": "TAMIL", "translation": "தொழில்சார் வாகனம் ஓட்டுதல்"},
                {"language": "TELUGU", "translation": "మంచి డ్రైవింగ్ కాదు"},
                {"language": "BENGALI", "translation": "অ-পেশাদার ড্রাইভিং"},
                {"language": "ODIA", "translation": "ଅପରିଶିଷ୍ଟ ଡ୍ରାଇଭିଂ"}
            ]
        },
        {
            "key": "ASKED_FOR_EXTRA",
            "sendPN": false,
            "priority": 2,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Asked for Extra"},
                {"language": "HINDI", "translation": "अतिरिक्त मांगा"},
                {"language": "KANNADA", "translation": "ಹೆಚ್ಚುವರಿ ಕೇಳಿದರು"},
                {"language": "TAMIL", "translation": "கூடுதல் கேட்டார்"},
                {"language": "TELUGU", "translation": "అదనంగా అడిగారు"},
                {"language": "BENGALI", "translation": "অতিরিক্ত চেয়েছেন"},
                {"language": "ODIA", "translation": "ଅତିରିକ୍ତ ମାଗିଲେ"}
            ]
        },
        {
            "key": "RUDE_DRIVER",
            "sendPN": true,
            "priority": 3,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Rude Driver"},
                {"language": "HINDI", "translation": "असभ्य चालक"},
                {"language": "KANNADA", "translation": "ಅಸಭ್ಯ ಚಾಲಕ"},
                {"language": "TAMIL", "translation": "முரட்டுத்தனமான ஓட்டுனர்"},
                {"language": "TELUGU", "translation": "మొరటు డ్రైవర్"},
                {"language": "BENGALI", "translation": "অভদ্র ড্রাইভার"},
                {"language": "ODIA", "translation": "ଅସଭ୍ୟ ଡ୍ରାଇଭର"}
            ]
        },
        {
            "key": "ROUTE_NOT_FOLLOWED",
            "sendPN": false,
            "priority": 4,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Route not Followed"},
                {"language": "HINDI", "translation": "रास्ता नहीं अपनाया"},
                {"language": "KANNADA", "translation": "ಮಾರ್ಗವನ್ನು ಅನುಸರಿಸಲಿಲ್ಲ"},
                {"language": "TAMIL", "translation": "வழி பின்பற்றப்படவில்லை"},
                {"language": "TELUGU", "translation": "మార్గం అనుసరించలేదు"},
                {"language": "BENGALI", "translation": "রুট অনুসরণ করেনি"},
                {"language": "ODIA", "translation": "ମାର୍ଗ ଅନୁସରଣ କରାଯାଇ ନାହିଁ"}
            ]
        },
        {
            "key": "APP_ISSUES",
            "sendPN": false,
            "priority": 5,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "App issues"},
                {"language": "HINDI", "translation": "ऐप की समस्याएं"},
                {"language": "KANNADA", "translation": "ಆಪ್ ಸಮಸ್ಯೆಗಳು"},
                {"language": "TAMIL", "translation": "ஆப் சிக்கல்கள்"},
                {"language": "TELUGU", "translation": "యాప్ సమస్యలు"},
                {"language": "BENGALI", "translation": "অ্যাপ সমস্যা"},
                {"language": "ODIA", "translation": "ଆପ୍ ସମସ୍ୟା"}
            ]
        },
        {
            "key": "FELT_UNSAFE",
            "sendPN": false,
            "priority": 6,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Felt Unsafe"},
                {"language": "HINDI", "translation": "असुरक्षित लगा"},
                {"language": "KANNADA", "translation": "ಅಸುರಕ್ಷಿತ ಭಾವನೆ"},
                {"language": "TAMIL", "translation": "பாதுகாப்பற்றதாக உணர்ந்தேன்"},
                {"language": "TELUGU", "translation": "అసురక్షితంగా అనిపించింది"},
                {"language": "BENGALI", "translation": "অনিরাপদ বোধ করছেন"},
                {"language": "ODIA", "translation": "ଅସୁରକ୍ଷିତ ମନେ ହେଲା"}
            ]
        },
        {
            "key": "UNCLEAN_VEHICLE",
            "sendPN": false,
            "priority": 7,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Unclean Vehicle"},
                {"language": "HINDI", "translation": "अशुद्ध वाहन"},
                {"language": "KANNADA", "translation": "ಅಶುದ್ಧ -ವಾಹನ"},
                {"language": "TAMIL", "translation": "அசுத்த வாகனம்"},
                {"language": "TELUGU", "translation": "అపరిశుభ్రమైన వాహనం"},
                {"language": "BENGALI", "translation": "অপরিষ্কার যানবাহন"},
                {"language": "ODIA", "translation": "ଅପରିଷ୍କାର ଯାନ"}
            ]
        },
        {
            "key": "AC_NOT_COOLING",
            "sendPN": false,
            "priority": 8,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "AC not cooling"},
                {"language": "HINDI", "translation": "एसी ठंडा नहीं हो रहा"},
                {"language": "KANNADA", "translation": "AC ತಣ್ಣಗಾಗುತ್ತಿಲ್ಲ"},
                {"language": "TAMIL", "translation": "AC குளிர்விக்கவில்லை"},
                {"language": "TELUGU", "translation": "AC చల్లగా లేదు"},
                {"language": "BENGALI", "translation": "AC ঠান্ডা হচ্ছে না"},
                {"language": "ODIA", "translation": "AC ଥଣ୍ଡା ହେଉନାହିଁ"}
            ]
        }
    ]'::jsonb
FROM atlas_app.merchant_operating_city moc
WHERE moc.state = 'West Bengal';


-- ============================================================================
-- DRIVER CATEGORY - Rating 4 (Good Experience) - WEST BENGAL ONLY
-- ============================================================================

INSERT INTO atlas_app.feedback_form (
    id, category_name, rating, question, question_translations, answer, answer_type,
    merchant_operating_city_id, merchant_id, badges
)
SELECT
    md5(random()::text || clock_timestamp()::text)::uuid as id,
    'DRIVER', 4,
    'Driver related feedback',
    '[{"language": "ENGLISH", "translation": "Driver related feedback"}, {"language": "HINDI", "translation": "ड्राइवर से संबंधित प्रतिक्रिया"}, {"language": "KANNADA", "translation": "ಚಾಲಕ ಸಂಬಂಧಿತ ಪ್ರತಿಕ್ರಿಯೆ"}, {"language": "TAMIL", "translation": "ஓட்டுனர் தொடர்பான கருத்து"}, {"language": "TELUGU", "translation": "డ్రైవర్ సంబంధిత అభిప్రాయం"}, {"language": "BENGALI", "translation": "ড্রাইভার সম্পর্কিত প্রতিক্রিয়া"}, {"language": "ODIA", "translation": "ଡ୍ରାଇଭର ସମ୍ବନ୍ଧୀୟ ମତାମତ"}]'::json,
    ARRAY['Polite Driver', 'Expert Driver', 'Asked for Extra Fare', 'Safe Ride', 'Uncomfortable Ride', 'Route not Followed', 'Clean Vehicle', 'Love the App <3', 'Comfortable ride']::text[],
    'Checkbox',
    moc.id, moc.merchant_id,
    '[
        {
            "key": "POLITE_DRIVER",
            "sendPN": false,
            "priority": 1,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Polite Driver"},
                {"language": "HINDI", "translation": "विनम्र चालक"},
                {"language": "KANNADA", "translation": "ಸಭ್ಯ ಚಾಲಕ"},
                {"language": "TAMIL", "translation": "பணிவான டிரைவர்"},
                {"language": "TELUGU", "translation": "మర్యాదపూర్వక డ్రైవర్"},
                {"language": "BENGALI", "translation": "ভদ্র ড্রাইভার"},
                {"language": "ODIA", "translation": "ଭଦ୍ର ଡ୍ରାଇଭର"}
            ]
        },
        {
            "key": "EXPERT_DRIVER",
            "sendPN": false,
            "priority": 2,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Expert Driver"},
                {"language": "HINDI", "translation": "कुशल ड्राइवर"},
                {"language": "KANNADA", "translation": "ಪರಿಣಿತ ಚಾಲಕ"},
                {"language": "TAMIL", "translation": "நிபுணர் டிரைவர்"},
                {"language": "TELUGU", "translation": "నిపుణ డ్రైవర్"},
                {"language": "BENGALI", "translation": "বিশেষজ্ঞ ড্রাইভার"},
                {"language": "ODIA", "translation": "ବିଶେଷଜ୍ଞ ଡ୍ରାଇଭର"}
            ]
        },
        {
            "key": "ASKED_FOR_EXTRA_FARE",
            "sendPN": false,
            "priority": 3,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Asked for Extra Fare"},
                {"language": "HINDI", "translation": "अतिरिक्त किराया मांगा"},
                {"language": "KANNADA", "translation": "ಹೆಚ್ಚುವರಿ ಶುಲ್ಕವನ್ನು ಕೇಳಿದೆ"},
                {"language": "TAMIL", "translation": "கூடுதல் கட்டணம் கேட்டார்கள்"},
                {"language": "TELUGU", "translation": "అదనపు ఛార్జీలు అడిగారు"},
                {"language": "BENGALI", "translation": "বাড়তি ভাড়া চেয়েছেন"},
                {"language": "ODIA", "translation": "ଅତିରିକ୍ତ ଭଡ଼ା ମାଗିଲେ"}
            ]
        },
        {
            "key": "SAFE_RIDE",
            "sendPN": false,
            "priority": 4,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Safe Ride"},
                {"language": "HINDI", "translation": "सुरक्षित सवारी"},
                {"language": "KANNADA", "translation": "ಸುರಕ್ಷಿತ ಸವಾರಿ"},
                {"language": "TAMIL", "translation": "பாதுகாப்பான சவாரி"},
                {"language": "TELUGU", "translation": "సురక్షిత రైడ్"},
                {"language": "BENGALI", "translation": "নিরাপদ যাত্রা"},
                {"language": "ODIA", "translation": "ସୁରକ୍ଷିତ ରାଇଡ୍"}
            ]
        },
        {
            "key": "UNCOMFORTABLE_RIDE",
            "sendPN": false,
            "priority": 5,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Uncomfortable Ride"},
                {"language": "HINDI", "translation": "असहज सवारी"},
                {"language": "KANNADA", "translation": "ಅನಾನುಕೂಲ ಸವಾರಿ"},
                {"language": "TAMIL", "translation": "சங்கடமான சவாரி"},
                {"language": "TELUGU", "translation": "అసౌకర్య రైడ్"},
                {"language": "BENGALI", "translation": "অস্বস্তিকর যাত্রা"},
                {"language": "ODIA", "translation": "ଅସହଜ ରାଇଡ୍"}
            ]
        },
        {
            "key": "ROUTE_NOT_FOLLOWED",
            "sendPN": false,
            "priority": 6,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Route not Followed"},
                {"language": "HINDI", "translation": "रास्ता नहीं अपनाया"},
                {"language": "KANNADA", "translation": "ಮಾರ್ಗವನ್ನು ಅನುಸರಿಸಲಿಲ್ಲ"},
                {"language": "TAMIL", "translation": "வழி பின்பற்றப்படவில்லை"},
                {"language": "TELUGU", "translation": "మార్గం అనుసరించలేదు"},
                {"language": "BENGALI", "translation": "রুট অনুসরণ করেনি"},
                {"language": "ODIA", "translation": "ମାର୍ଗ ଅନୁସରଣ କରାଯାଇ ନାହିଁ"}
            ]
        },
        {
            "key": "CLEAN_VEHICLE",
            "sendPN": false,
            "priority": 7,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Clean Vehicle"},
                {"language": "HINDI", "translation": "स्वच्छ वाहन"},
                {"language": "KANNADA", "translation": "ಶುದ್ಧ ವಾಹನ"},
                {"language": "TAMIL", "translation": "சுத்தமான வாகனம்"},
                {"language": "TELUGU", "translation": "శుభ్రమైన వాహనం"},
                {"language": "BENGALI", "translation": "পরিচ্ছন্ন যানবাহন"},
                {"language": "ODIA", "translation": "ପରିଷ୍କାର ଯାନ"}
            ]
        },
        {
            "key": "LOVE_THE_APP",
            "sendPN": false,
            "priority": 8,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Love the App <3"},
                {"language": "HINDI", "translation": "ऐप पसंद है <3"},
                {"language": "KANNADA", "translation": "ಆಪ್ ಇಷ್ಟ <3"},
                {"language": "TAMIL", "translation": "ஆப்பை விரும்புகிறேன் <3"},
                {"language": "TELUGU", "translation": "యాప్ ఇష్టం <3"},
                {"language": "BENGALI", "translation": "অ্যাপ পছন্দ <3"},
                {"language": "ODIA", "translation": "ଆପ୍ ପସନ୍ଦ <3"}
            ]
        }
    ]'::jsonb
FROM atlas_app.merchant_operating_city moc
WHERE moc.state = 'West Bengal';


-- ============================================================================
-- DRIVER CATEGORY - Rating 5 (Excellent Experience) - WEST BENGAL ONLY
-- ============================================================================

INSERT INTO atlas_app.feedback_form (
    id, category_name, rating, question, question_translations, answer, answer_type,
    merchant_operating_city_id, merchant_id, badges
)
SELECT
    md5(random()::text || clock_timestamp()::text)::uuid as id,
    'DRIVER', 5,
    'Driver related feedback',
    '[{"language": "ENGLISH", "translation": "Driver related feedback"}, {"language": "HINDI", "translation": "ड्राइवर से संबंधित प्रतिक्रिया"}, {"language": "KANNADA", "translation": "ಚಾಲಕ ಸಂಬಂಧಿತ ಪ್ರತಿಕ್ರಿಯೆ"}, {"language": "TAMIL", "translation": "ஓட்டுனர் தொடர்பான கருத்து"}, {"language": "TELUGU", "translation": "డ్రైవర్ సంబంధిత అభిప్రాయం"}, {"language": "BENGALI", "translation": "ড্রাইভার সম্পর্কিত প্রতিক্রিয়া"}, {"language": "ODIA", "translation": "ଡ୍ରାଇଭର ସମ୍ବନ୍ଧୀୟ ମତାମତ"}]'::json,
    ARRAY['Polite Driver', 'Expert Driving', 'On Time', 'Skilled Navigator', 'Safe Ride', 'Clean Vehicle', 'Love the App <3', 'Comfortable ride']::text[],
    'Checkbox',
    moc.id, moc.merchant_id,
    '[
        {
            "key": "POLITE_DRIVER",
            "sendPN": false,
            "priority": 1,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Polite Driver"},
                {"language": "HINDI", "translation": "विनम्र चालक"},
                {"language": "KANNADA", "translation": "ಸಭ್ಯ ಚಾಲಕ"},
                {"language": "TAMIL", "translation": "பணிவான டிரைவர்"},
                {"language": "TELUGU", "translation": "మర్యాదపూర్వక డ్రైవర్"},
                {"language": "BENGALI", "translation": "ভদ্র ড্রাইভার"},
                {"language": "ODIA", "translation": "ଭଦ୍ର ଡ୍ରାଇଭର"}
            ]
        },
        {
            "key": "EXPERT_DRIVING",
            "sendPN": false,
            "priority": 2,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Expert Driving"},
                {"language": "HINDI", "translation": "कुशल ड्राइविंग"},
                {"language": "KANNADA", "translation": "ಪರಿಣಿತ ಚಾಲನೆ"},
                {"language": "TAMIL", "translation": "டிரைவர் நன்றாக ஓட்டினார்"},
                {"language": "TELUGU", "translation": "నిపుణమైన డ్రైవింగ్"},
                {"language": "BENGALI", "translation": "এক্সপার্ট ড্রাইভিং"},
                {"language": "ODIA", "translation": "ବିଶେଷଜ୍ଞ ଡ୍ରାଇଭିଂ"}
            ]
        },
        {
            "key": "ON_TIME",
            "sendPN": false,
            "priority": 3,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "On Time"},
                {"language": "HINDI", "translation": "समय पर"},
                {"language": "KANNADA", "translation": "ಸಮಯಕ್ಕೆ"},
                {"language": "TAMIL", "translation": "சரியான நேரத்தில்"},
                {"language": "TELUGU", "translation": "సమయానికి"},
                {"language": "BENGALI", "translation": "সময়মতো"},
                {"language": "ODIA", "translation": "ସମୟରେ"}
            ]
        },
        {
            "key": "SKILLED_NAVIGATOR",
            "sendPN": false,
            "priority": 4,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Skilled Navigator"},
                {"language": "HINDI", "translation": "कुशल नाविक"},
                {"language": "KANNADA", "translation": "ನುರಿತ ನ್ಯಾವಿಗೇಟರ್"},
                {"language": "TAMIL", "translation": "திறமையான நேவிகேட்டர்"},
                {"language": "TELUGU", "translation": "నైపుణ్యం కలిగిన నావిగేటర్"},
                {"language": "BENGALI", "translation": "দক্ষ নেভিগেটর"},
                {"language": "ODIA", "translation": "ଦକ୍ଷ ନାଭିଗେଟର୍"}
            ]
        },
        {
            "key": "SAFE_RIDE",
            "sendPN": false,
            "priority": 5,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Safe Ride"},
                {"language": "HINDI", "translation": "सुरक्षित सवारी"},
                {"language": "KANNADA", "translation": "ಸುರಕ್ಷಿತ ಸವಾರಿ"},
                {"language": "TAMIL", "translation": "பாதுகாப்பான சவாரி"},
                {"language": "TELUGU", "translation": "సురక్షిత రైడ్"},
                {"language": "BENGALI", "translation": "নিরাপদ যাত্রা"},
                {"language": "ODIA", "translation": "ସୁରକ୍ଷିତ ରାଇଡ୍"}
            ]
        },
        {
            "key": "CLEAN_VEHICLE",
            "sendPN": false,
            "priority": 6,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Clean Vehicle"},
                {"language": "HINDI", "translation": "स्वच्छ वाहन"},
                {"language": "KANNADA", "translation": "ಶುದ್ಧ ವಾಹನ"},
                {"language": "TAMIL", "translation": "சுத்தமான வாகனம்"},
                {"language": "TELUGU", "translation": "శుభ్రమైన వాహనం"},
                {"language": "BENGALI", "translation": "পরিচ্ছন্ন যানবাহন"},
                {"language": "ODIA", "translation": "ପରିଷ୍କାର ଯାନ"}
            ]
        },
        {
            "key": "LOVE_THE_APP",
            "sendPN": false,
            "priority": 7,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Love the App <3"},
                {"language": "HINDI", "translation": "ऐप पसंद है <3"},
                {"language": "KANNADA", "translation": "ಆಪ್ ಇಷ್ಟ <3"},
                {"language": "TAMIL", "translation": "ஆப்பை விரும்புகிறேன் <3"},
                {"language": "TELUGU", "translation": "యాప్ ఇష్టం <3"},
                {"language": "BENGALI", "translation": "অ্যাপ পছন্দ <3"},
                {"language": "ODIA", "translation": "ଆପ୍ ପସନ୍ଦ <3"}
            ]
        },
        {
            "key": "COMFORTABLE_RIDE",
            "sendPN": false,
            "priority": 8,
            "contentWithTranslations": [
                {"language": "ENGLISH", "translation": "Comfortable ride"},
                {"language": "HINDI", "translation": "आरामदायक सवारी"},
                {"language": "KANNADA", "translation": "ಆರಾಮದಾಯಕ ಸವಾರಿ"},
                {"language": "TAMIL", "translation": "வசதியான சவாரி"},
                {"language": "TELUGU", "translation": "సౌకర్యవంతమైన రైడ్"},
                {"language": "BENGALI", "translation": "আরামদায়ক যাত্রা"},
                {"language": "ODIA", "translation": "ଆରାମଦାୟକ ରାଇଡ୍"}
            ]
        }
    ]'::jsonb
FROM atlas_app.merchant_operating_city moc
WHERE moc.state = 'West Bengal';
