-- WhatsApp bot copy, Phase 1: 6 pilot fields (flexiArrived splits into two DB
-- keys for its empty/non-empty-OTP branches) x English/Hindi/Kannada only, as
-- global (merchant_operating_city_id = NULL) rows so any city can override
-- later with zero code change. Gujarati/Tamil/Telugu are intentionally NOT
-- seeded in this phase and keep rendering the static compiled copy.
-- {{0}}/{{1}} are positional placeholders substituted by
-- WhatsappBot.Adapter.Translations.substitute.
INSERT INTO atlas_app.translations (id, message_key, language, message, merchant_operating_city_id, created_at, updated_at)
VALUES
  -- welcome
  (gen_random_uuid()::text, 'wa_bot_welcome', 'ENGLISH', '🙏 Namaskara! I''m your Namma Yatri assistant

Ready to book an auto?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_welcome', 'HINDI', '🙏 नमस्ते! मैं आपका Namma Yatri सहायक हूँ

ऑटो बुक करने के लिए तैयार हैं?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_welcome', 'KANNADA', '🙏 ನಮಸ್ಕಾರ! ನಾನು ನಿಮ್ಮ Namma Yatri ಸಹಾಯಕ

ಆಟೋ ಬುಕ್ ಮಾಡಲು ಸಿದ್ಧರಾ?', NULL, now(), now()),

  -- chooseLanguage
  (gen_random_uuid()::text, 'wa_bot_chooseLanguage', 'ENGLISH', '🌐 Language', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_chooseLanguage', 'HINDI', '🌐 भाषा', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_chooseLanguage', 'KANNADA', '🌐 ಭಾಷೆ', NULL, now(), now()),

  -- setupFailed ({{0}} = error text)
  (gen_random_uuid()::text, 'wa_bot_setupFailed', 'ENGLISH', 'Setup failed: {{0}}
Send "book" to try again.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_setupFailed', 'HINDI', 'सेटअप विफल: {{0}}
दोबारा कोशिश करने के लिए "book" भेजें।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_setupFailed', 'KANNADA', 'ಸೆಟಪ್ ವಿಫಲವಾಗಿದೆ: {{0}}
ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಲು "book" ಕಳುಹಿಸಿ.', NULL, now(), now()),

  -- languageUpdated ({{0}} = new language display name)
  (gen_random_uuid()::text, 'wa_bot_languageUpdated', 'ENGLISH', '✅ Language changed to *{{0}}*.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_languageUpdated', 'HINDI', '✅ भाषा *{{0}}* में बदल दी गई।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_languageUpdated', 'KANNADA', '✅ ಭಾಷೆಯನ್ನು *{{0}}* ಗೆ ಬದಲಾಯಿಸಲಾಗಿದೆ.', NULL, now(), now()),

  -- flexiFareRate ({{0}} = base fare, {{1}} = per-km fare)
  (gen_random_uuid()::text, 'wa_bot_flexiFareRate', 'ENGLISH', '🛺 Metered auto · from ₹{{0}} + ₹{{1}}/km', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareRate', 'HINDI', '🛺 मीटर ऑटो · ₹{{0}} + ₹{{1}}/किमी से', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareRate', 'KANNADA', '🛺 ಮೀಟರ್ ಆಟೋ · ₹{{0}} + ₹{{1}}/ಕಿಮೀ ಇಂದ', NULL, now(), now()),

  -- flexiArrived, no-OTP branch
  (gen_random_uuid()::text, 'wa_bot_flexiArrived_noOtp', 'ENGLISH', '🛺 Your auto has arrived!
Please meet your driver at the pickup point.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiArrived_noOtp', 'HINDI', '🛺 आपका ऑटो आ गया है!
कृपया पिकअप पॉइंट पर अपने ड्राइवर से मिलें।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiArrived_noOtp', 'KANNADA', '🛺 ನಿಮ್ಮ ಆಟೋ ಬಂದಿದೆ!
ದಯವಿಟ್ಟು ಪಿಕಪ್ ಪಾಯಿಂಟ್‌ನಲ್ಲಿ ನಿಮ್ಮ ಚಾಲಕರನ್ನು ಭೇಟಿಯಾಗಿ.', NULL, now(), now()),

  -- flexiArrived, with-OTP branch ({{0}} = OTP)
  (gen_random_uuid()::text, 'wa_bot_flexiArrived_withOtp', 'ENGLISH', '🛺 Your auto has arrived!
Share OTP with the driver to start.

🔑 OTP: *{{0}}* ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiArrived_withOtp', 'HINDI', '🛺 आपका ऑटो आ गया है!
राइड शुरू करने के लिए ड्राइवर को OTP बताएँ।

🔑 OTP: *{{0}}* ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiArrived_withOtp', 'KANNADA', '🛺 ನಿಮ್ಮ ಆಟೋ ಬಂದಿದೆ!
ರೈಡ್ ಪ್ರಾರಂಭಿಸಲು ಚಾಲಕರಿಗೆ OTP ಹೇಳಿ.

🔑 OTP: *{{0}}* ', NULL, now(), now())
ON CONFLICT (message_key, language) WHERE merchant_operating_city_id IS NULL DO NOTHING;
