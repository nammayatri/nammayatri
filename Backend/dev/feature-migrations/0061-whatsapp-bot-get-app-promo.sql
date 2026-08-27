-- WhatsApp bot copy: getAppButton/getAppMessage, the "Get the App" promo
-- button shown on the ride-completion message (WhatsappBot.Messages.buildEnded)
-- and its follow-up when tapped (Engine.hs's "get_app" arm). {{0}} = the
-- merchant's appDownloadUrl (MetaBotCfg), substituted by
-- WhatsappBot.Adapter.Translations.substitute.
--
-- Gujarati/Kannada/Tamil/Telugu copy here is freshly authored (not translated
-- from an existing verified source, unlike most other seeded fields) --
-- worth a native-speaker review before this is relied on in production.
INSERT INTO atlas_app.translations (id, message_key, language, message, merchant_operating_city_id, created_at, updated_at)
VALUES
  -- getAppButton
  (gen_random_uuid()::text, 'wa_bot_getAppButton', 'ENGLISH', '📲 Get the App', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_getAppButton', 'HINDI', '📲 ऐप पाएं', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_getAppButton', 'GUJARATI', '📲 એપ મેળવો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_getAppButton', 'KANNADA', '📲 ಆ್ಯಪ್ ಪಡೆಯಿರಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_getAppButton', 'TAMIL', '📲 ஆப்ஸ் பெறுங்கள்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_getAppButton', 'TELUGU', '📲 యాప్ పొందండి', NULL, now(), now()),

  -- getAppMessage ({{0}} = appDownloadUrl)
  (gen_random_uuid()::text, 'wa_bot_getAppMessage', 'ENGLISH', '📲 Get the full Namma Yatri experience!

✨ Track your ride live
📍 Save your favorite places
🎁 Exclusive in-app offers
⚡ Faster bookings, one tap away

Download now: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_getAppMessage', 'HINDI', '📲 पूरा Namma Yatri अनुभव पाएं!

✨ अपनी राइड लाइव ट्रैक करें
📍 अपनी पसंदीदा जगहें सेव करें
🎁 ऐप में खास ऑफर्स
⚡ तेज़ बुकिंग, बस एक टैप में

अभी डाउनलोड करें: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_getAppMessage', 'GUJARATI', '📲 સંપૂર્ણ Namma Yatri અનુભવ મેળવો!

✨ તમારી રાઈડ લાઈવ ટ્રેક કરો
📍 તમારા મનપસંદ સ્થળો સેવ કરો
🎁 એપમાં ખાસ ઓફર્સ
⚡ ઝડપી બુકિંગ, એક ટેપમાં

હમણાં ડાઉનલોડ કરો: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_getAppMessage', 'KANNADA', '📲 ಸಂಪೂರ್ಣ Namma Yatri ಅನುಭವ ಪಡೆಯಿರಿ!

✨ ನಿಮ್ಮ ರೈಡ್ ಅನ್ನು ಲೈವ್ ಟ್ರ್ಯಾಕ್ ಮಾಡಿ
📍 ನಿಮ್ಮ ಮೆಚ್ಚಿನ ಸ್ಥಳಗಳನ್ನು ಉಳಿಸಿ
🎁 ಆ್ಯಪ್‌ನಲ್ಲಿ ವಿಶೇಷ ಆಫರ್‌ಗಳು
⚡ ವೇಗದ ಬುಕಿಂಗ್, ಒಂದೇ ಟ್ಯಾಪ್‌ನಲ್ಲಿ

ಈಗ ಡೌನ್‌ಲೋಡ್ ಮಾಡಿ: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_getAppMessage', 'TAMIL', '📲 முழு Namma Yatri அனுபவத்தைப் பெறுங்கள்!

✨ உங்கள் சவாரியை நேரலையில் கண்காணிக்கவும்
📍 உங்கள் விருப்பமான இடங்களைச் சேமிக்கவும்
🎁 ஆப்ஸில் சிறப்பு சலுகைகள்
⚡ வேகமான முன்பதிவு, ஒரே தட்டில்

இப்போது பதிவிறக்கவும்: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_getAppMessage', 'TELUGU', '📲 పూర్తి Namma Yatri అనుభవాన్ని పొందండి!

✨ మీ రైడ్‌ను ప్రత్యక్షంగా ట్రాక్ చేయండి
📍 మీకు ఇష్టమైన ప్రదేశాలను సేవ్ చేయండి
🎁 యాప్‌లో ప్రత్యేక ఆఫర్‌లు
⚡ వేగవంతమైన బుకింగ్, ఒక్క ట్యాప్‌లో

ఇప్పుడే డౌన్‌లోడ్ చేయండి: {{0}}', NULL, now(), now())
ON CONFLICT (message_key, language) WHERE merchant_operating_city_id IS NULL DO NOTHING;
