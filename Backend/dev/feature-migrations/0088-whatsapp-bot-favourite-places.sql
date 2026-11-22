-- WhatsApp bot copy: favourite places. Riders with saved places get a list
-- (Favourites / Share new location / Add a favourite) at pickup and drop, and
-- riders with none get an "Add a favourite" nudge under the location request,
-- and can save a place from the pickup confirmation (WhatsappBot.Flow.Favourites).
-- Same favourites as the Namma Yatri app (atlas_app.saved_req_location).
--
-- Run BEFORE deploying the rider-app build that reads it: a key missing in every
-- language fails the bot's translation load (WhatsappBot.Adapter.Translations).
--
-- Non-English copy here is freshly authored -- worth a native-speaker review
-- before this is relied on in production.
INSERT INTO atlas_app.translations (id, message_key, language, message, merchant_operating_city_id, created_at, updated_at) VALUES
  -- favChoosePickup
  (gen_random_uuid()::text, 'wa_bot_favChoosePickup', 'ENGLISH', 'Choose pickup', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favChoosePickup', 'HINDI', 'पिकअप चुनें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favChoosePickup', 'GUJARATI', 'પિકઅપ પસંદ કરો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favChoosePickup', 'KANNADA', 'ಪಿಕಪ್ ಆಯ್ಕೆಮಾಡಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favChoosePickup', 'TAMIL', 'பிக்கப் தேர்வு', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favChoosePickup', 'TELUGU', 'పికప్ ఎంచుకోండి', NULL, now(), now()),
  -- favChooseDrop
  (gen_random_uuid()::text, 'wa_bot_favChooseDrop', 'ENGLISH', 'Choose drop', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favChooseDrop', 'HINDI', 'ड्रॉप चुनें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favChooseDrop', 'GUJARATI', 'ડ્રોપ પસંદ કરો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favChooseDrop', 'KANNADA', 'ಡ್ರಾಪ್ ಆಯ್ಕೆಮಾಡಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favChooseDrop', 'TAMIL', 'டிராப் தேர்வு', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favChooseDrop', 'TELUGU', 'డ్రాప్ ఎంచుకోండి', NULL, now(), now()),
  -- favSectionSaved
  (gen_random_uuid()::text, 'wa_bot_favSectionSaved', 'ENGLISH', 'Favourites', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSectionSaved', 'HINDI', 'पसंदीदा', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSectionSaved', 'GUJARATI', 'મનપસંદ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSectionSaved', 'KANNADA', 'ಮೆಚ್ಚಿನವು', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSectionSaved', 'TAMIL', 'பிடித்தவை', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSectionSaved', 'TELUGU', 'ఇష్టమైనవి', NULL, now(), now()),
  -- favSectionOther
  (gen_random_uuid()::text, 'wa_bot_favSectionOther', 'ENGLISH', 'Other', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSectionOther', 'HINDI', 'अन्य', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSectionOther', 'GUJARATI', 'અન્ય', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSectionOther', 'KANNADA', 'ಇತರೆ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSectionOther', 'TAMIL', 'மற்றவை', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSectionOther', 'TELUGU', 'ఇతరాలు', NULL, now(), now()),
  -- favShareNew
  (gen_random_uuid()::text, 'wa_bot_favShareNew', 'ENGLISH', '📍 Share new location', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favShareNew', 'HINDI', '📍 नई लोकेशन भेजें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favShareNew', 'GUJARATI', '📍 નવું લોકેશન મોકલો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favShareNew', 'KANNADA', '📍 ಹೊಸ ಸ್ಥಳ ಕಳುಹಿಸಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favShareNew', 'TAMIL', '📍 புதிய இடம் அனுப்பு', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favShareNew', 'TELUGU', '📍 కొత్త లొకేషన్ పంపండి', NULL, now(), now()),
  -- favAddNew
  (gen_random_uuid()::text, 'wa_bot_favAddNew', 'ENGLISH', '➕ Add a favourite', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favAddNew', 'HINDI', '➕ पसंदीदा जोड़ें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favAddNew', 'GUJARATI', '➕ મનપસંદ ઉમેરો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favAddNew', 'KANNADA', '➕ ಮೆಚ್ಚಿನದು ಸೇರಿಸಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favAddNew', 'TAMIL', '➕ பிடித்த இடம் சேர்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favAddNew', 'TELUGU', '➕ ఇష్టమైనది జోడించు', NULL, now(), now()),
  -- favAddPrompt
  (gen_random_uuid()::text, 'wa_bot_favAddPrompt', 'ENGLISH', 'Save places you use often, like Home or Work, and book them in one tap.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favAddPrompt', 'HINDI', 'अक्सर जाने वाली जगहें, जैसे घर या ऑफिस, सेव करें और एक टैप में बुक करें।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favAddPrompt', 'GUJARATI', 'વારંવાર જતા સ્થળો, જેમ કે ઘર કે ઓફિસ, સેવ કરો અને એક ટેપમાં બુક કરો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favAddPrompt', 'KANNADA', 'ಮನೆ, ಕೆಲಸದಂತಹ ಆಗಾಗ ಹೋಗುವ ಸ್ಥಳಗಳನ್ನು ಉಳಿಸಿ, ಒಂದೇ ಟ್ಯಾಪ್‌ನಲ್ಲಿ ಬುಕ್ ಮಾಡಿ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favAddPrompt', 'TAMIL', 'வீடு, அலுவலகம் போன்ற அடிக்கடி செல்லும் இடங்களைச் சேமித்து, ஒரே தட்டலில் புக் செய்யுங்கள்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favAddPrompt', 'TELUGU', 'ఇల్లు, ఆఫీస్ లాంటి తరచూ వెళ్ళే స్థలాలను సేవ్ చేసి, ఒక్క ట్యాప్‌లో బుక్ చేయండి.', NULL, now(), now()),
  -- favSaveButton
  (gen_random_uuid()::text, 'wa_bot_favSaveButton', 'ENGLISH', '⭐ Save place', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSaveButton', 'HINDI', '⭐ जगह सेव करें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSaveButton', 'GUJARATI', '⭐ જગ્યા સેવ કરો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSaveButton', 'KANNADA', '⭐ ಸ್ಥಳ ಉಳಿಸಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSaveButton', 'TAMIL', '⭐ இடத்தைச் சேமி', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSaveButton', 'TELUGU', '⭐ స్థలం సేవ్ చేయండి', NULL, now(), now()),
  -- favAddPinPrompt
  (gen_random_uuid()::text, 'wa_bot_favAddPinPrompt', 'ENGLISH', 'Share the location you want to save as a favourite.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favAddPinPrompt', 'HINDI', 'वह लोकेशन भेजें जिसे आप पसंदीदा के रूप में सेव करना चाहते हैं।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favAddPinPrompt', 'GUJARATI', 'જે લોકેશન મનપસંદ તરીકે સેવ કરવું છે તે મોકલો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favAddPinPrompt', 'KANNADA', 'ಮೆಚ್ಚಿನದಾಗಿ ಉಳಿಸಲು ಬಯಸುವ ಸ್ಥಳವನ್ನು ಕಳುಹಿಸಿ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favAddPinPrompt', 'TAMIL', 'பிடித்த இடமாகச் சேமிக்க விரும்பும் இடத்தை அனுப்புங்கள்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favAddPinPrompt', 'TELUGU', 'ఇష్టమైనదిగా సేవ్ చేయాలనుకునే లొకేషన్‌ను పంపండి.', NULL, now(), now()),
  -- favNamePrompt
  (gen_random_uuid()::text, 'wa_bot_favNamePrompt', 'ENGLISH', 'Save this place as?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNamePrompt', 'HINDI', 'इस जगह को किस नाम से सेव करें?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNamePrompt', 'GUJARATI', 'આ જગ્યા કયા નામે સેવ કરવી?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNamePrompt', 'KANNADA', 'ಈ ಸ್ಥಳವನ್ನು ಯಾವ ಹೆಸರಿನಲ್ಲಿ ಉಳಿಸಬೇಕು?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNamePrompt', 'TAMIL', 'இந்த இடத்தை எந்தப் பெயரில் சேமிக்க வேண்டும்?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNamePrompt', 'TELUGU', 'ఈ స్థలాన్ని ఏ పేరుతో సేవ్ చేయాలి?', NULL, now(), now()),
  -- favNameHome
  (gen_random_uuid()::text, 'wa_bot_favNameHome', 'ENGLISH', '🏠 Home', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNameHome', 'HINDI', '🏠 घर', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNameHome', 'GUJARATI', '🏠 ઘર', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNameHome', 'KANNADA', '🏠 ಮನೆ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNameHome', 'TAMIL', '🏠 வீடு', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNameHome', 'TELUGU', '🏠 ఇల్లు', NULL, now(), now()),
  -- favNameWork
  (gen_random_uuid()::text, 'wa_bot_favNameWork', 'ENGLISH', '💼 Work', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNameWork', 'HINDI', '💼 ऑफिस', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNameWork', 'GUJARATI', '💼 ઓફિસ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNameWork', 'KANNADA', '💼 ಕೆಲಸ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNameWork', 'TAMIL', '💼 அலுவலகம்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNameWork', 'TELUGU', '💼 ఆఫీస్', NULL, now(), now()),
  -- favNameOther
  (gen_random_uuid()::text, 'wa_bot_favNameOther', 'ENGLISH', '✏️ Other', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNameOther', 'HINDI', '✏️ अन्य', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNameOther', 'GUJARATI', '✏️ અન્ય', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNameOther', 'KANNADA', '✏️ ಇತರೆ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNameOther', 'TAMIL', '✏️ மற்றவை', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favNameOther', 'TELUGU', '✏️ ఇతరం', NULL, now(), now()),
  -- favTypeName
  (gen_random_uuid()::text, 'wa_bot_favTypeName', 'ENGLISH', 'Type a name for this place, for example *Gym*.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favTypeName', 'HINDI', 'इस जगह का नाम लिखें, जैसे *Gym*।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favTypeName', 'GUJARATI', 'આ જગ્યાનું નામ લખો, જેમ કે *Gym*.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favTypeName', 'KANNADA', 'ಈ ಸ್ಥಳಕ್ಕೆ ಹೆಸರು ಬರೆಯಿರಿ, ಉದಾ: *Gym*.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favTypeName', 'TAMIL', 'இந்த இடத்திற்கு ஒரு பெயரை டைப் செய்யுங்கள், உதா: *Gym*.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favTypeName', 'TELUGU', 'ఈ స్థలానికి ఒక పేరు టైప్ చేయండి, ఉదా: *Gym*.', NULL, now(), now()),
  -- favSaved
  (gen_random_uuid()::text, 'wa_bot_favSaved', 'ENGLISH', '⭐ Saved as *{{0}}*.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSaved', 'HINDI', '⭐ *{{0}}* के रूप में सेव किया गया।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSaved', 'GUJARATI', '⭐ *{{0}}* તરીકે સેવ થયું.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSaved', 'KANNADA', '⭐ *{{0}}* ಎಂದು ಉಳಿಸಲಾಗಿದೆ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSaved', 'TAMIL', '⭐ *{{0}}* என்று சேமிக்கப்பட்டது.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSaved', 'TELUGU', '⭐ *{{0}}* గా సేవ్ చేయబడింది.', NULL, now(), now()),
  -- favReplacePrompt
  (gen_random_uuid()::text, 'wa_bot_favReplacePrompt', 'ENGLISH', 'You already have a favourite called *{{0}}*. Replace it with this place?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favReplacePrompt', 'HINDI', 'आपके पास पहले से *{{0}}* नाम की पसंदीदा जगह है। क्या इसे इस जगह से बदलें?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favReplacePrompt', 'GUJARATI', 'તમારી પાસે પહેલેથી *{{0}}* નામની મનપસંદ જગ્યા છે. તેને આ જગ્યાથી બદલવી છે?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favReplacePrompt', 'KANNADA', 'ನಿಮ್ಮಲ್ಲಿ ಈಗಾಗಲೇ *{{0}}* ಎಂಬ ಮೆಚ್ಚಿನ ಸ್ಥಳವಿದೆ. ಅದನ್ನು ಈ ಸ್ಥಳದಿಂದ ಬದಲಿಸಬೇಕೇ?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favReplacePrompt', 'TAMIL', '*{{0}}* என்ற பெயரில் ஏற்கனவே ஒரு பிடித்த இடம் உள்ளது. அதை இந்த இடத்தால் மாற்றவா?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favReplacePrompt', 'TELUGU', 'మీకు ఇప్పటికే *{{0}}* అనే ఇష్టమైన స్థలం ఉంది. దాన్ని ఈ స్థలంతో మార్చాలా?', NULL, now(), now()),
  -- favReplaceYes
  (gen_random_uuid()::text, 'wa_bot_favReplaceYes', 'ENGLISH', 'Yes, replace', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favReplaceYes', 'HINDI', 'हाँ, बदलें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favReplaceYes', 'GUJARATI', 'હા, બદલો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favReplaceYes', 'KANNADA', 'ಹೌದು, ಬದಲಿಸಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favReplaceYes', 'TAMIL', 'ஆம், மாற்று', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favReplaceYes', 'TELUGU', 'అవును, మార్చు', NULL, now(), now()),
  -- favReplaceNo
  (gen_random_uuid()::text, 'wa_bot_favReplaceNo', 'ENGLISH', 'No, new name', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favReplaceNo', 'HINDI', 'नहीं, नया नाम', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favReplaceNo', 'GUJARATI', 'ના, નવું નામ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favReplaceNo', 'KANNADA', 'ಇಲ್ಲ, ಹೊಸ ಹೆಸರು', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favReplaceNo', 'TAMIL', 'இல்லை, புதிய பெயர்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favReplaceNo', 'TELUGU', 'వద్దు, కొత్త పేరు', NULL, now(), now()),
  -- favSaveFailed
  (gen_random_uuid()::text, 'wa_bot_favSaveFailed', 'ENGLISH', 'Couldn''t save this place. Please try again later.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSaveFailed', 'HINDI', 'यह जगह सेव नहीं हो सकी। कृपया बाद में फिर कोशिश करें।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSaveFailed', 'GUJARATI', 'આ જગ્યા સેવ થઈ શકી નથી. કૃપા કરીને પછી ફરી પ્રયાસ કરો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSaveFailed', 'KANNADA', 'ಈ ಸ್ಥಳವನ್ನು ಉಳಿಸಲಾಗಲಿಲ್ಲ. ದಯವಿಟ್ಟು ನಂತರ ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSaveFailed', 'TAMIL', 'இந்த இடத்தைச் சேமிக்க முடியவில்லை. பிறகு மீண்டும் முயற்சிக்கவும்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_favSaveFailed', 'TELUGU', 'ఈ స్థలాన్ని సేవ్ చేయలేకపోయాం. దయచేసి తర్వాత మళ్ళీ ప్రయత్నించండి.', NULL, now(), now())
ON CONFLICT (message_key, language) WHERE merchant_operating_city_id IS NULL DO NOTHING;
