-- WhatsApp bot copy, Phase 2: remaining fields not covered by Phase 1 (0047),
-- across all 6 languages (English/Hindi/Gujarati/Kannada/Tamil/Telugu), as
-- global (merchant_operating_city_id = NULL) rows so any city can override
-- later with zero code change. Also includes a gap-fill for Phase 1's own 6
-- pilot fields (welcome, chooseLanguage, setupFailed, languageUpdated,
-- flexiFareRate, flexiArrived) in Gujarati/Tamil/Telugu, which 0047 never
-- seeded (English/Hindi/Kannada only) -- see the dedicated block near the end
-- of this file for why that gap became urgent (batch-1 wiring removed the
-- static fallback, so a missing row is now a hard error, not a silent
-- degrade).
--
-- Text extracted VERBATIM from the static WhatsappBot.I18n.{En,Hi,Gu,Kn,Ta,Te}
-- modules (not newly translated) -- those files already had full copy for every
-- field in every language; this migration only copies it into the DB so it CAN
-- be read from there. Fields with a `Maybe Text` argument (cancelConfirmWithDriver,
-- flexiFareFinal) split into two message_keys, one per branch, same technique
-- 0047 used for flexiArrived's empty/non-empty-OTP branches.
--
-- {{0}}/{{1}} are positional placeholders substituted by
-- WhatsappBot.Adapter.Translations.substitute.
--
-- IMPORTANT: this migration adds DB DATA ONLY. As of writing, WhatsappBot.Adapter.
-- Translations's buildLanguageStrings reads the 6 pilot fields (already wired
-- since Phase 1) plus batch 1 (auth/registration: welcomeBack, personNotFound,
-- otpSent, invalidOtp, resendOtp, otpResent, otpResendFailed, otpVerified,
-- otpVerifyFailed) -- every other field in this file still needs its own
-- Haskell resolveField wiring (one call + one record-override line each,
-- mirroring the fields already wired) added in a later batch before it takes
-- effect.
-- before any of this seed data actually takes effect.
INSERT INTO atlas_app.translations (id, message_key, language, message, merchant_operating_city_id, created_at, updated_at)
VALUES
  -- languageName
  (gen_random_uuid()::text, 'wa_bot_languageName', 'ENGLISH', 'English', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_languageName', 'HINDI', 'Hindi', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_languageName', 'GUJARATI', 'Gujarati', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_languageName', 'KANNADA', 'Kannada', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_languageName', 'TAMIL', 'Tamil', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_languageName', 'TELUGU', 'Telugu', NULL, now(), now()),

  -- nativeLanguageName
  (gen_random_uuid()::text, 'wa_bot_nativeLanguageName', 'ENGLISH', 'English', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_nativeLanguageName', 'HINDI', 'हिन्दी', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_nativeLanguageName', 'GUJARATI', 'ગુજરાતી', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_nativeLanguageName', 'KANNADA', 'ಕನ್ನಡ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_nativeLanguageName', 'TAMIL', 'தமிழ்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_nativeLanguageName', 'TELUGU', 'తెలుగు', NULL, now(), now()),

  -- bookARide
  (gen_random_uuid()::text, 'wa_bot_bookARide', 'ENGLISH', '🚕 Book a Ride', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_bookARide', 'HINDI', '🚕 राइड बुक करें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_bookARide', 'GUJARATI', '🚕 રાઈડ બુક કરો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_bookARide', 'KANNADA', '🚕 ರೈಡ್ ಬುಕ್ ಮಾಡಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_bookARide', 'TAMIL', '🚕 சவாரி புக் செய்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_bookARide', 'TELUGU', '🚕 రైడ్ బుక్ చేయండి', NULL, now(), now()),

  -- trackRide
  (gen_random_uuid()::text, 'wa_bot_trackRide', 'ENGLISH', '📍 Track Ride', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_trackRide', 'HINDI', '📍 राइड ट्रैक करें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_trackRide', 'GUJARATI', '📍 રાઈડ ટ્રેક કરો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_trackRide', 'KANNADA', '📍 ರೈಡ್ ಟ್ರ್ಯಾಕ್ ಮಾಡಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_trackRide', 'TAMIL', '📍 சவாரி கண்காணி', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_trackRide', 'TELUGU', '📍 రైడ్ ట్రాక్ చేయండి', NULL, now(), now()),

  -- selectLanguage
  (gen_random_uuid()::text, 'wa_bot_selectLanguage', 'ENGLISH', '🌐 Choose your preferred language:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_selectLanguage', 'HINDI', '🌐 अपनी पसंदीदा भाषा चुनें:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_selectLanguage', 'GUJARATI', '🌐 તમારી પસંદગીની ભાષા પસંદ કરો:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_selectLanguage', 'KANNADA', '🌐 ನಿಮ್ಮ ಆದ್ಯತೆಯ ಭಾಷೆಯನ್ನು ಆಯ್ಕೆ ಮಾಡಿ:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_selectLanguage', 'TAMIL', '🌐 உங்கள் விருப்பமான மொழியைத் தேர்ந்தெடுக்கவும்:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_selectLanguage', 'TELUGU', '🌐 మీకు నచ్చిన భాషను ఎంచుకోండి:', NULL, now(), now()),

  -- moreLanguages
  (gen_random_uuid()::text, 'wa_bot_moreLanguages', 'ENGLISH', '➕ More languages', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreLanguages', 'HINDI', '➕ और भाषाएँ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreLanguages', 'GUJARATI', '➕ વધુ ભાષાઓ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreLanguages', 'KANNADA', '➕ ಹೆಚ್ಚಿನ ಭಾಷೆಗಳು', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreLanguages', 'TAMIL', '➕ மேலும் மொழிகள்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreLanguages', 'TELUGU', '➕ మరిన్ని భాషలు', NULL, now(), now()),

  -- welcomeBack
  (gen_random_uuid()::text, 'wa_bot_welcomeBack', 'ENGLISH', 'Welcome back, {{0}}! 👋', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_welcomeBack', 'HINDI', 'वापसी पर स्वागत है, {{0}}! 👋', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_welcomeBack', 'GUJARATI', 'પાછા સ્વાગત છે, {{0}}! 👋', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_welcomeBack', 'KANNADA', 'ಮತ್ತೆ ಸ್ವಾಗತ, {{0}}! 👋', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_welcomeBack', 'TAMIL', 'மீண்டும் வரவேற்கிறோம், {{0}}! 👋', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_welcomeBack', 'TELUGU', 'తిరిగి స్వాగతం, {{0}}! 👋', NULL, now(), now()),

  -- personNotFound
  (gen_random_uuid()::text, 'wa_bot_personNotFound', 'ENGLISH', 'Looks like you''re new! Let''s get you set up.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_personNotFound', 'HINDI', 'लगता है आप नए हैं! चलिए आपका अकाउंट सेटअप करते हैं।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_personNotFound', 'GUJARATI', 'લાગે છે તમે નવા છો! ચાલો તમારું એકાઉન્ટ સેટઅપ કરીએ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_personNotFound', 'KANNADA', 'ನೀವು ಹೊಸಬರು ಎಂದು ತೋರುತ್ತದೆ! ನಿಮ್ಮ ಖಾತೆಯನ್ನು ಸೆಟಪ್ ಮಾಡೋಣ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_personNotFound', 'TAMIL', 'நீங்கள் புதியவர் போல் தெரிகிறது! உங்கள் கணக்கை அமைக்கலாம்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_personNotFound', 'TELUGU', 'మీరు కొత్తవారు అనిపిస్తోంది! మీ ఖాతాను సెటప్ చేద్దాం.', NULL, now(), now()),

  -- otpSent
  (gen_random_uuid()::text, 'wa_bot_otpSent', 'ENGLISH', 'An OTP has been sent to your phone. Please enter the OTP:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpSent', 'HINDI', 'आपके फ़ोन पर OTP भेजा गया है। कृपया OTP दर्ज करें:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpSent', 'GUJARATI', 'તમારા ફોન પર OTP મોકલવામાં આવ્યો છે. કૃપા કરીને OTP દાખલ કરો:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpSent', 'KANNADA', 'ನಿಮ್ಮ ಫೋನ್‌ಗೆ OTP ಕಳುಹಿಸಲಾಗಿದೆ. ದಯವಿಟ್ಟು OTP ನಮೂದಿಸಿ:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpSent', 'TAMIL', 'உங்கள் தொலைபேசிக்கு OTP அனுப்பப்பட்டுள்ளது. OTP ஐ உள்ளிடவும்:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpSent', 'TELUGU', 'మీ ఫోన్‌కు OTP పంపబడింది. దయచేసి OTP నమోదు చేయండి:', NULL, now(), now()),

  -- invalidOtp
  (gen_random_uuid()::text, 'wa_bot_invalidOtp', 'ENGLISH', 'That doesn''t look like a valid OTP. Please enter the 4 or 6 digit code:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_invalidOtp', 'HINDI', 'यह वैध OTP नहीं लगता। कृपया 4 या 6 अंकों का कोड दर्ज करें:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_invalidOtp', 'GUJARATI', 'આ માન્ય OTP નથી. કૃપા કરીને 4 અથવા 6 અંકનો કોડ દાખલ કરો:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_invalidOtp', 'KANNADA', 'ಇದು ಸರಿಯಾದ OTP ಅಲ್ಲ. ದಯವಿಟ್ಟು 4 ಅಥವಾ 6 ಅಂಕಿಯ ಕೋಡ್ ನಮೂದಿಸಿ:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_invalidOtp', 'TAMIL', 'இது சரியான OTP இல்லை. 4 அல்லது 6 இலக்க குறியீட்டை உள்ளிடவும்:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_invalidOtp', 'TELUGU', 'ఇది చెల్లుబాటు అయ్యే OTP కాదు. దయచేసి 4 లేదా 6 అంకెల కోడ్ నమోదు చేయండి:', NULL, now(), now()),

  -- resendOtp
  (gen_random_uuid()::text, 'wa_bot_resendOtp', 'ENGLISH', '🔄 Resend OTP', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_resendOtp', 'HINDI', '🔄 OTP दोबारा भेजें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_resendOtp', 'GUJARATI', '🔄 OTP ફરીથી મોકલો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_resendOtp', 'KANNADA', '🔄 OTP ಮರುಕಳುಹಿಸಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_resendOtp', 'TAMIL', '🔄 OTP மீண்டும் அனுப்பு', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_resendOtp', 'TELUGU', '🔄 OTP మళ్ళీ పంపు', NULL, now(), now()),

  -- otpResent
  (gen_random_uuid()::text, 'wa_bot_otpResent', 'ENGLISH', 'OTP has been resent. Please check your phone:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpResent', 'HINDI', 'OTP दोबारा भेजा गया है। कृपया अपना फ़ोन चेक करें:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpResent', 'GUJARATI', 'OTP ફરીથી મોકલવામાં આવ્યો છે. કૃપા કરીને તમારો ફોન તપાસો:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpResent', 'KANNADA', 'OTP ಮರುಕಳುಹಿಸಲಾಗಿದೆ. ದಯವಿಟ್ಟು ನಿಮ್ಮ ಫೋನ್ ಪರಿಶೀಲಿಸಿ:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpResent', 'TAMIL', 'OTP மீண்டும் அனுப்பப்பட்டுள்ளது. உங்கள் தொலைபேசியைச் சரிபார்க்கவும்:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpResent', 'TELUGU', 'OTP మళ్ళీ పంపబడింది. దయచేసి మీ ఫోన్ తనిఖీ చేయండి:', NULL, now(), now()),

  -- otpResendFailed
  (gen_random_uuid()::text, 'wa_bot_otpResendFailed', 'ENGLISH', 'Could not resend OTP: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpResendFailed', 'HINDI', 'OTP दोबारा नहीं भेज पाए: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpResendFailed', 'GUJARATI', 'OTP ફરીથી મોકલી શકાયો નહીં: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpResendFailed', 'KANNADA', 'OTP ಮರುಕಳುಹಿಸಲು ಸಾಧ್ಯವಾಗಲಿಲ್ಲ: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpResendFailed', 'TAMIL', 'OTP மீண்டும் அனுப்ப முடியவில்லை: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpResendFailed', 'TELUGU', 'OTP మళ్ళీ పంపలేకపోయాము: {{0}}', NULL, now(), now()),

  -- otpVerified
  (gen_random_uuid()::text, 'wa_bot_otpVerified', 'ENGLISH', 'Phone verified successfully!', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpVerified', 'HINDI', 'फ़ोन सत्यापित हो गया!', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpVerified', 'GUJARATI', 'ફોન સફળતાપૂર્વક ચકાસાયો!', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpVerified', 'KANNADA', 'ಫೋನ್ ಯಶಸ್ವಿಯಾಗಿ ಪರಿಶೀಲಿಸಲಾಗಿದೆ!', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpVerified', 'TAMIL', 'தொலைபேசி வெற்றிகரமாக சரிபார்க்கப்பட்டது!', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpVerified', 'TELUGU', 'ఫోన్ విజయవంతంగా ధృవీకరించబడింది!', NULL, now(), now()),

  -- otpVerifyFailed
  (gen_random_uuid()::text, 'wa_bot_otpVerifyFailed', 'ENGLISH', 'OTP verification failed: {{0}}
Please try again:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpVerifyFailed', 'HINDI', 'OTP सत्यापन विफल: {{0}}
कृपया दोबारा कोशिश करें:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpVerifyFailed', 'GUJARATI', 'OTP ચકાસણી નિષ્ફળ: {{0}}
કૃપા કરીને ફરીથી પ્રયાસ કરો:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpVerifyFailed', 'KANNADA', 'OTP ಪರಿಶೀಲನೆ ವಿಫಲವಾಗಿದೆ: {{0}}
ದಯವಿಟ್ಟು ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpVerifyFailed', 'TAMIL', 'OTP சரிபார்ப்பு தோல்வி: {{0}}
மீண்டும் முயற்சிக்கவும்:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpVerifyFailed', 'TELUGU', 'OTP ధృవీకరణ విఫలమైంది: {{0}}
దయచేసి మళ్ళీ ప్రయత్నించండి:', NULL, now(), now()),

  -- noPlacesFound
  (gen_random_uuid()::text, 'wa_bot_noPlacesFound', 'ENGLISH', 'No places found. Try a different search:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noPlacesFound', 'HINDI', 'कोई जगह नहीं मिली। कुछ और खोजें:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noPlacesFound', 'GUJARATI', 'કોઈ સ્થાન મળ્યું નથી. અલગ શોધ અજમાવો:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noPlacesFound', 'KANNADA', 'ಯಾವುದೇ ಸ್ಥಳ ಸಿಗಲಿಲ್ಲ. ಬೇರೆ ಹೆಸರಿನಿಂದ ಹುಡುಕಿ:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noPlacesFound', 'TAMIL', 'இடங்கள் எதுவும் கிடைக்கவில்லை. வேறு தேடலை முயற்சிக்கவும்:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noPlacesFound', 'TELUGU', 'ప్రదేశాలు కనుగొనబడలేదు. వేరే పేరుతో ప్రయత్నించండి:', NULL, now(), now()),

  -- track
  (gen_random_uuid()::text, 'wa_bot_track', 'ENGLISH', '📲 Track:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_track', 'HINDI', '📲 ट्रैक:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_track', 'GUJARATI', '📲 ટ્રેક:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_track', 'KANNADA', '📲 ಟ್ರ್ಯಾಕ್:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_track', 'TAMIL', '📲 கண்காணி:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_track', 'TELUGU', '📲 ట్రాక్:', NULL, now(), now()),

  -- callDriver
  (gen_random_uuid()::text, 'wa_bot_callDriver', 'ENGLISH', '📞 Call Driver', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_callDriver', 'HINDI', '📞 कॉल करें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_callDriver', 'GUJARATI', '📞 કૉલ કરો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_callDriver', 'KANNADA', '📞 ಕರೆ ಮಾಡಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_callDriver', 'TAMIL', '📞 அழைக்கவும்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_callDriver', 'TELUGU', '📞 కాల్ చేయండి', NULL, now(), now()),

  -- cancelRide
  (gen_random_uuid()::text, 'wa_bot_cancelRide', 'ENGLISH', '❌ Cancel Ride', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelRide', 'HINDI', '❌ रद्द करें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelRide', 'GUJARATI', '❌ રદ કરો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelRide', 'KANNADA', '❌ ರದ್ದು ಮಾಡಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelRide', 'TAMIL', '❌ ரத்து செய்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelRide', 'TELUGU', '❌ రద్దు చేయండి', NULL, now(), now()),

  -- driverLabel
  (gen_random_uuid()::text, 'wa_bot_driverLabel', 'ENGLISH', '👤 Driver: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_driverLabel', 'HINDI', '👤 ड्राइवर: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_driverLabel', 'GUJARATI', '👤 ડ્રાઈવર: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_driverLabel', 'KANNADA', '👤 ಚಾಲಕ: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_driverLabel', 'TAMIL', '👤 டிரைவர்: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_driverLabel', 'TELUGU', '👤 డ్రైవర్: *{{0}}*', NULL, now(), now()),

  -- vehicleLabel
  (gen_random_uuid()::text, 'wa_bot_vehicleLabel', 'ENGLISH', '🔢 Vehicle: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_vehicleLabel', 'HINDI', '🔢 गाड़ी: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_vehicleLabel', 'GUJARATI', '🔢 વાહન: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_vehicleLabel', 'KANNADA', '🔢 ವಾಹನ: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_vehicleLabel', 'TAMIL', '🔢 வாகனம்: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_vehicleLabel', 'TELUGU', '🔢 వాహనం: *{{0}}*', NULL, now(), now()),

  -- phoneLabel
  (gen_random_uuid()::text, 'wa_bot_phoneLabel', 'ENGLISH', '📞 Phone: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_phoneLabel', 'HINDI', '📞 फ़ोन: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_phoneLabel', 'GUJARATI', '📞 ફોન: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_phoneLabel', 'KANNADA', '📞 ಫೋನ್: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_phoneLabel', 'TAMIL', '📞 தொலைபேசி: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_phoneLabel', 'TELUGU', '📞 ఫోన్: *{{0}}*', NULL, now(), now()),

  -- otpLabel
  (gen_random_uuid()::text, 'wa_bot_otpLabel', 'ENGLISH', '🔑 OTP: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpLabel', 'HINDI', '🔑 OTP: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpLabel', 'GUJARATI', '🔑 OTP: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpLabel', 'KANNADA', '🔑 OTP: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpLabel', 'TAMIL', '🔑 OTP: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_otpLabel', 'TELUGU', '🔑 OTP: *{{0}}*', NULL, now(), now()),

  -- driverPhone
  (gen_random_uuid()::text, 'wa_bot_driverPhone', 'ENGLISH', '📞 Driver''s number: *{{0}}*

You can call them directly.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_driverPhone', 'HINDI', '📞 ड्राइवर का नंबर: *{{0}}*

आप उन्हें सीधे कॉल कर सकते हैं।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_driverPhone', 'GUJARATI', '📞 ડ્રાઈવરનો નંબર: *{{0}}*

તમે તેમને સીધો કૉલ કરી શકો છો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_driverPhone', 'KANNADA', '📞 ಚಾಲಕರ ಸಂಖ್ಯೆ: *{{0}}*

ನೀವು ನೇರವಾಗಿ ಕರೆ ಮಾಡಬಹುದು.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_driverPhone', 'TAMIL', '📞 டிரைவரின் எண்: *{{0}}*

நீங்கள் நேரடியாக அழைக்கலாம்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_driverPhone', 'TELUGU', '📞 డ్రైవర్ నంబర్: *{{0}}*

మీరు వారికి నేరుగా కాల్ చేయవచ్చు.', NULL, now(), now()),

  -- driverDetailsNotAvailable
  (gen_random_uuid()::text, 'wa_bot_driverDetailsNotAvailable', 'ENGLISH', 'Driver details are not available yet. Please try again in a moment.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_driverDetailsNotAvailable', 'HINDI', 'ड्राइवर की जानकारी अभी उपलब्ध नहीं है। कृपया कुछ देर बाद कोशिश करें।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_driverDetailsNotAvailable', 'GUJARATI', 'ડ્રાઈવરની વિગતો હજુ ઉપલબ્ધ નથી. કૃપા કરીને થોડીવાર પછી ફરીથી પ્રયાસ કરો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_driverDetailsNotAvailable', 'KANNADA', 'ಚಾಲಕರ ವಿವರಗಳು ಇನ್ನೂ ಲಭ್ಯವಿಲ್ಲ. ದಯವಿಟ್ಟು ಸ್ವಲ್ಪ ಸಮಯದ ನಂತರ ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_driverDetailsNotAvailable', 'TAMIL', 'டிரைவர் விவரங்கள் இன்னும் கிடைக்கவில்லை. சிறிது நேரம் கழித்து மீண்டும் முயற்சிக்கவும்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_driverDetailsNotAvailable', 'TELUGU', 'డ్రైవర్ వివరాలు ఇంకా అందుబాటులో లేవు. దయచేసి కొద్దిసేపట్లో మళ్ళీ ప్రయత్నించండి.', NULL, now(), now()),

  -- noActiveRide
  (gen_random_uuid()::text, 'wa_bot_noActiveRide', 'ENGLISH', 'No active ride found.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noActiveRide', 'HINDI', 'कोई चालू राइड नहीं मिली।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noActiveRide', 'GUJARATI', 'કોઈ સક્રિય રાઈડ મળી નથી.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noActiveRide', 'KANNADA', 'ಸಕ್ರಿಯ ರೈಡ್ ಕಂಡುಬಂದಿಲ್ಲ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noActiveRide', 'TAMIL', 'செயலில் உள்ள சவாரி எதுவும் கிடைக்கவில்லை.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noActiveRide', 'TELUGU', 'యాక్టివ్ రైడ్ కనుగొనబడలేదు.', NULL, now(), now()),

  -- mainMenu
  (gen_random_uuid()::text, 'wa_bot_mainMenu', 'ENGLISH', '🏠 Main menu', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_mainMenu', 'HINDI', '🏠 मुख्य मेनू', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_mainMenu', 'GUJARATI', '🏠 મુખ્ય મેનૂ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_mainMenu', 'KANNADA', '🏠 ಮುಖ್ಯ ಮೆನು', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_mainMenu', 'TAMIL', '🏠 முதன்மை பட்டியல்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_mainMenu', 'TELUGU', '🏠 ప్రధాన మెనూ', NULL, now(), now()),

  -- activeRide
  (gen_random_uuid()::text, 'wa_bot_activeRide', 'ENGLISH', '📍 Active Ride
', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_activeRide', 'HINDI', '📍 चालू राइड
', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_activeRide', 'GUJARATI', '📍 સક્રિય રાઈડ
', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_activeRide', 'KANNADA', '📍 ಸಕ್ರಿಯ ರೈಡ್
', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_activeRide', 'TAMIL', '📍 செயலில் உள்ள சவாரி
', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_activeRide', 'TELUGU', '📍 యాక్టివ్ రైడ్
', NULL, now(), now()),

  -- noActiveRidesBook
  (gen_random_uuid()::text, 'wa_bot_noActiveRidesBook', 'ENGLISH', '🔍 No active rides found.

Would you like to book one?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noActiveRidesBook', 'HINDI', '🔍 कोई चालू राइड नहीं मिली।

क्या आप राइड बुक करना चाहेंगे?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noActiveRidesBook', 'GUJARATI', '🔍 કોઈ સક્રિય રાઈડ મળી નથી.

શું તમે એક બુક કરવા માગો છો?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noActiveRidesBook', 'KANNADA', '🔍 ಸಕ್ರಿಯ ರೈಡ್‌ಗಳು ಕಂಡುಬಂದಿಲ್ಲ.

ಒಂದನ್ನು ಬುಕ್ ಮಾಡಲು ಬಯಸುತ್ತೀರಾ?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noActiveRidesBook', 'TAMIL', '🔍 செயலில் உள்ள சவாரிகள் எதுவும் கிடைக்கவில்லை.

ஒன்றை புக் செய்ய விரும்புகிறீர்களா?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noActiveRidesBook', 'TELUGU', '🔍 యాక్టివ్ రైడ్‌లు కనుగొనబడలేదు.

ఒకటి బుక్ చేయాలనుకుంటున్నారా?', NULL, now(), now()),

  -- cancelConfirm
  (gen_random_uuid()::text, 'wa_bot_cancelConfirm', 'ENGLISH', '⚠️ Are you sure you want to cancel your ride?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelConfirm', 'HINDI', '⚠️ क्या आप वाकई अपनी राइड रद्द करना चाहते हैं?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelConfirm', 'GUJARATI', '⚠️ શું તમે ખરેખર તમારી રાઈડ રદ કરવા માગો છો?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelConfirm', 'KANNADA', '⚠️ ನಿಮ್ಮ ರೈಡ್ ರದ್ದು ಮಾಡಲು ಖಚಿತವಾಗಿದ್ದೀರಾ?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelConfirm', 'TAMIL', '⚠️ உங்கள் சவாரியை ரத்து செய்ய விரும்புகிறீர்களா?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelConfirm', 'TELUGU', '⚠️ మీరు ఖచ్చితంగా మీ రైడ్ రద్దు చేయాలనుకుంటున్నారా?', NULL, now(), now()),

  -- cancelConfirmWithDriver
  (gen_random_uuid()::text, 'wa_bot_cancelConfirmWithDriver_noVehicle', 'ENGLISH', '⚠️ Cancel ride with *{{0}}*?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelConfirmWithDriver_noVehicle', 'HINDI', '⚠️ *{{0}}* के साथ राइड रद्द करें?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelConfirmWithDriver_noVehicle', 'GUJARATI', '⚠️ *{{0}}* સાથેની રાઈડ રદ કરવી છે?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelConfirmWithDriver_noVehicle', 'KANNADA', '⚠️ *{{0}}* ಜೊತೆಗಿನ ರೈಡ್ ರದ್ದು ಮಾಡಬೇಕೇ?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelConfirmWithDriver_noVehicle', 'TAMIL', '⚠️ *{{0}}* உடனான சவாரியை ரத்து செய்யவா?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelConfirmWithDriver_noVehicle', 'TELUGU', '⚠️ *{{0}}* తో రైడ్ రద్దు చేయాలా?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelConfirmWithDriver_withVehicle', 'ENGLISH', '⚠️ Cancel ride with *{{0}}* ({{1}})?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelConfirmWithDriver_withVehicle', 'HINDI', '⚠️ *{{0}}* ({{1}}) के साथ राइड रद्द करें?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelConfirmWithDriver_withVehicle', 'GUJARATI', '⚠️ *{{0}}* ({{1}}) સાથેની રાઈડ રદ કરવી છે?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelConfirmWithDriver_withVehicle', 'KANNADA', '⚠️ *{{0}}* ({{1}}) ಜೊತೆಗಿನ ರೈಡ್ ರದ್ದು ಮಾಡಬೇಕೇ?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelConfirmWithDriver_withVehicle', 'TAMIL', '⚠️ *{{0}}* ({{1}}) உடனான சவாரியை ரத்து செய்யவா?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelConfirmWithDriver_withVehicle', 'TELUGU', '⚠️ *{{0}}* ({{1}}) తో రైడ్ రద్దు చేయాలా?', NULL, now(), now()),

  -- yesCancelIt
  (gen_random_uuid()::text, 'wa_bot_yesCancelIt', 'ENGLISH', '✅ Yes, cancel it', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_yesCancelIt', 'HINDI', '✅ हाँ, रद्द करें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_yesCancelIt', 'GUJARATI', '✅ હા, રદ કરો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_yesCancelIt', 'KANNADA', '✅ ಹೌದು, ರದ್ದು ಮಾಡಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_yesCancelIt', 'TAMIL', '✅ ஆம், ரத்து செய்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_yesCancelIt', 'TELUGU', '✅ అవును, రద్దు చేయండి', NULL, now(), now()),

  -- noKeepIt
  (gen_random_uuid()::text, 'wa_bot_noKeepIt', 'ENGLISH', '🔙 No, keep it', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noKeepIt', 'HINDI', '🔙 नहीं, रहने दें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noKeepIt', 'GUJARATI', '🔙 ના, રાખો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noKeepIt', 'KANNADA', '🔙 ಬೇಡ, ಉಳಿಸಿಕೊಳ್ಳಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noKeepIt', 'TAMIL', '🔙 வேண்டாம், தொடரவும்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noKeepIt', 'TELUGU', '🔙 వద్దు, ఉంచండి', NULL, now(), now()),

  -- rideCancelled
  (gen_random_uuid()::text, 'wa_bot_rideCancelled', 'ENGLISH', 'Ride cancelled. ✅', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideCancelled', 'HINDI', 'राइड रद्द हो गई। ✅', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideCancelled', 'GUJARATI', 'રાઈડ રદ થઈ. ✅', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideCancelled', 'KANNADA', 'ರೈಡ್ ರದ್ದಾಗಿದೆ. ✅', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideCancelled', 'TAMIL', 'சவாரி ரத்து செய்யப்பட்டது. ✅', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideCancelled', 'TELUGU', 'రైడ్ రద్దు చేయబడింది. ✅', NULL, now(), now()),

  -- rideCompleted
  (gen_random_uuid()::text, 'wa_bot_rideCompleted', 'ENGLISH', 'This ride has already been completed and cannot be cancelled.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideCompleted', 'HINDI', 'यह राइड पहले ही पूरी हो चुकी है और रद्द नहीं की जा सकती।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideCompleted', 'GUJARATI', 'આ રાઈડ પહેલેથી પૂર્ણ થઈ ગઈ છે અને રદ કરી શકાતી નથી.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideCompleted', 'KANNADA', 'ಈ ರೈಡ್ ಈಗಾಗಲೇ ಪೂರ್ಣಗೊಂಡಿದೆ ಮತ್ತು ರದ್ದು ಮಾಡಲು ಸಾಧ್ಯವಿಲ್ಲ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideCompleted', 'TAMIL', 'இந்த சவாரி ஏற்கனவே முடிவடைந்துவிட்டது, ரத்து செய்ய இயலாது.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideCompleted', 'TELUGU', 'ఈ రైడ్ ఇప్పటికే పూర్తయింది మరియు రద్దు చేయలేము.', NULL, now(), now()),

  -- rideAlreadyCancelled
  (gen_random_uuid()::text, 'wa_bot_rideAlreadyCancelled', 'ENGLISH', 'This ride is already cancelled.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideAlreadyCancelled', 'HINDI', 'यह राइड पहले ही रद्द हो चुकी है।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideAlreadyCancelled', 'GUJARATI', 'આ રાઈડ પહેલેથી રદ થઈ ગઈ છે.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideAlreadyCancelled', 'KANNADA', 'ಈ ರೈಡ್ ಈಗಾಗಲೇ ರದ್ದಾಗಿದೆ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideAlreadyCancelled', 'TAMIL', 'இந்த சவாரி ஏற்கனவே ரத்து செய்யப்பட்டுள்ளது.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideAlreadyCancelled', 'TELUGU', 'ఈ రైడ్ ఇప్పటికే రద్దు చేయబడింది.', NULL, now(), now()),

  -- rideInProgress
  (gen_random_uuid()::text, 'wa_bot_rideInProgress', 'ENGLISH', '⚠️ Your ride is already in progress and cannot be cancelled.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideInProgress', 'HINDI', '⚠️ आपकी राइड चल रही है और रद्द नहीं की जा सकती।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideInProgress', 'GUJARATI', '⚠️ તમારી રાઈડ પહેલેથી ચાલુ છે અને રદ કરી શકાતી નથી.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideInProgress', 'KANNADA', '⚠️ ನಿಮ್ಮ ರೈಡ್ ಈಗಾಗಲೇ ಪ್ರಗತಿಯಲ್ಲಿದೆ ಮತ್ತು ರದ್ದು ಮಾಡಲು ಸಾಧ್ಯವಿಲ್ಲ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideInProgress', 'TAMIL', '⚠️ உங்கள் சவாரி ஏற்கனவே நடைபெற்றுக்கொண்டிருக்கிறது, ரத்து செய்ய இயலாது.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideInProgress', 'TELUGU', '⚠️ మీ రైడ్ ఇప్పటికే ప్రగతిలో ఉంది మరియు రద్దు చేయలేము.', NULL, now(), now()),

  -- cancelFailed
  (gen_random_uuid()::text, 'wa_bot_cancelFailed', 'ENGLISH', 'Could not cancel: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelFailed', 'HINDI', 'रद्द नहीं हो सकी: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelFailed', 'GUJARATI', 'રદ કરી શકાયું નહીં: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelFailed', 'KANNADA', 'ರದ್ದು ಮಾಡಲು ಸಾಧ್ಯವಾಗಲಿಲ್ಲ: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelFailed', 'TAMIL', 'ரத்து செய்ய இயலவில்லை: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelFailed', 'TELUGU', 'రద్దు చేయలేకపోయాము: {{0}}', NULL, now(), now()),

  -- cancelled
  (gen_random_uuid()::text, 'wa_bot_cancelled', 'ENGLISH', 'Cancelled.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelled', 'HINDI', 'रद्द हो गया।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelled', 'GUJARATI', 'રદ થયું.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelled', 'KANNADA', 'ರದ್ದಾಗಿದೆ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelled', 'TAMIL', 'ரத்து செய்யப்பட்டது.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_cancelled', 'TELUGU', 'రద్దు చేయబడింది.', NULL, now(), now()),

  -- whatToDo
  (gen_random_uuid()::text, 'wa_bot_whatToDo', 'ENGLISH', '

What would you like to do?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_whatToDo', 'HINDI', '

आप क्या करना चाहेंगे?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_whatToDo', 'GUJARATI', '

તમે શું કરવા માગો છો?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_whatToDo', 'KANNADA', '

ನೀವು ಏನು ಮಾಡಲು ಬಯಸುತ್ತೀರಿ?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_whatToDo', 'TAMIL', '

நீங்கள் என்ன செய்ய விரும்புகிறீர்கள்?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_whatToDo', 'TELUGU', '

మీరు ఏమి చేయాలనుకుంటున్నారు?', NULL, now(), now()),

  -- rideNotStarted
  (gen_random_uuid()::text, 'wa_bot_rideNotStarted', 'ENGLISH', '🕐 Ride not started yet — driver is on the way.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideNotStarted', 'HINDI', '🕐 राइड अभी शुरू नहीं हुई — ड्राइवर रास्ते में है।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideNotStarted', 'GUJARATI', '🕐 રાઈડ હજુ શરૂ થઈ નથી — ડ્રાઈવર રસ્તામાં છે.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideNotStarted', 'KANNADA', '🕐 ರೈಡ್ ಇನ್ನೂ ಪ್ರಾರಂಭವಾಗಿಲ್ಲ — ಚಾಲಕ ಬರುತ್ತಿದ್ದಾರೆ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideNotStarted', 'TAMIL', '🕐 சவாரி இன்னும் தொடங்கவில்லை — டிரைவர் வழியில் இருக்கிறார்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideNotStarted', 'TELUGU', '🕐 రైడ్ ఇంకా ప్రారంభం కాలేదు — డ్రైవర్ వస్తున్నారు.', NULL, now(), now()),

  -- rideInProgressStatus
  (gen_random_uuid()::text, 'wa_bot_rideInProgressStatus', 'ENGLISH', '🚗 Ride is in progress.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideInProgressStatus', 'HINDI', '🚗 राइड चल रही है।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideInProgressStatus', 'GUJARATI', '🚗 રાઈડ ચાલુ છે.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideInProgressStatus', 'KANNADA', '🚗 ರೈಡ್ ಪ್ರಗತಿಯಲ್ಲಿದೆ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideInProgressStatus', 'TAMIL', '🚗 சவாரி நடைபெற்றுக்கொண்டிருக்கிறது.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideInProgressStatus', 'TELUGU', '🚗 రైడ్ ప్రగతిలో ఉంది.', NULL, now(), now()),

  -- sosButton
  (gen_random_uuid()::text, 'wa_bot_sosButton', 'ENGLISH', '🚨 SOS', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosButton', 'HINDI', '🚨 SOS', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosButton', 'GUJARATI', '🚨 SOS', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosButton', 'KANNADA', '🚨 SOS', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosButton', 'TAMIL', '🚨 SOS', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosButton', 'TELUGU', '🚨 SOS', NULL, now(), now()),

  -- call112Button
  (gen_random_uuid()::text, 'wa_bot_call112Button', 'ENGLISH', '📞 Call 112', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_call112Button', 'HINDI', '📞 112 पर कॉल करें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_call112Button', 'GUJARATI', '📞 112 પર કૉલ કરો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_call112Button', 'KANNADA', '📞 112 ಗೆ ಕರೆ ಮಾಡಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_call112Button', 'TAMIL', '📞 112 அழைக்கவும்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_call112Button', 'TELUGU', '📞 112 కి కాల్ చేయండి', NULL, now(), now()),

  -- sosConfirm
  (gen_random_uuid()::text, 'wa_bot_sosConfirm', 'ENGLISH', '⚠️ Are you sure you want to trigger an SOS alert? This will notify emergency contacts and Namma Yatri safety team.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosConfirm', 'HINDI', '⚠️ क्या आप SOS अलर्ट भेजना चाहते हैं? यह आपातकालीन संपर्कों और नम्मा यात्री सुरक्षा टीम को सूचित करेगा।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosConfirm', 'GUJARATI', '⚠️ શું તમે SOS અલર્ટ મોકલવા માગો છો? આ ઇમરજન્સી કોન્ટેક્ટ્સ અને નમ્મા યાત્રી સેફ્ટી ટીમને જાણ કરશે.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosConfirm', 'KANNADA', '⚠️ SOS ಎಚ್ಚರಿಕೆಯನ್ನು ಕಳುಹಿಸಲು ಖಚಿತವಾಗಿದ್ದೀರಾ? ಇದು ತುರ್ತು ಸಂಪರ್ಕಗಳು ಮತ್ತು ನಮ್ಮ ಯಾತ್ರಿ ಸುರಕ್ಷತಾ ತಂಡಕ್ಕೆ ತಿಳಿಸುತ್ತದೆ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosConfirm', 'TAMIL', '⚠️ SOS எச்சரிக்கையை அனுப்ப விரும்புகிறீர்களா? இது அவசர தொடர்புகள் மற்றும் நம்ம யாத்ரி பாதுகாப்பு குழுவிற்கு தெரிவிக்கும்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosConfirm', 'TELUGU', '⚠️ SOS అలర్ట్ పంపాలనుకుంటున్నారా? ఇది ఎమర్జెన్సీ కాంటాక్ట్‌లకు మరియు నమ్మ యాత్రి సేఫ్టీ టీమ్‌కు తెలియజేస్తుంది.', NULL, now(), now()),

  -- yesTriggerSOS
  (gen_random_uuid()::text, 'wa_bot_yesTriggerSOS', 'ENGLISH', '🚨 Yes, trigger SOS', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_yesTriggerSOS', 'HINDI', '🚨 हाँ, SOS भेजें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_yesTriggerSOS', 'GUJARATI', '🚨 હા, SOS મોકલો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_yesTriggerSOS', 'KANNADA', '🚨 ಹೌದು, SOS ಕಳುಹಿಸಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_yesTriggerSOS', 'TAMIL', '🚨 ஆம், SOS அனுப்பு', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_yesTriggerSOS', 'TELUGU', '🚨 అవును, SOS పంపండి', NULL, now(), now()),

  -- noGoBack
  (gen_random_uuid()::text, 'wa_bot_noGoBack', 'ENGLISH', '🔙 No, go back', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noGoBack', 'HINDI', '🔙 नहीं, वापस जाएँ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noGoBack', 'GUJARATI', '🔙 ના, પાછા જાઓ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noGoBack', 'KANNADA', '🔙 ಬೇಡ, ಹಿಂದೆ ಹೋಗಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noGoBack', 'TAMIL', '🔙 வேண்டாம், திரும்பு', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_noGoBack', 'TELUGU', '🔙 వద్దు, వెనక్కి వెళ్ళండి', NULL, now(), now()),

  -- sosTriggered
  (gen_random_uuid()::text, 'wa_bot_sosTriggered', 'ENGLISH', '🚨 SOS alert has been triggered. Stay safe — help is on the way.

You can also call 112 for immediate emergency assistance.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosTriggered', 'HINDI', '🚨 SOS अलर्ट भेज दिया गया है। सुरक्षित रहें — मदद आ रही है।

आप तुरंत आपातकालीन सहायता के लिए 112 पर भी कॉल कर सकते हैं।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosTriggered', 'GUJARATI', '🚨 SOS અલર્ટ મોકલાઈ ગયો છે. સુરક્ષિત રહો — મદદ આવી રહી છે.

તમે તાત્કાલિક ઇમરજન્સી સહાય માટે 112 પર પણ કૉલ કરી શકો છો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosTriggered', 'KANNADA', '🚨 SOS ಎಚ್ಚರಿಕೆ ಕಳುಹಿಸಲಾಗಿದೆ. ಸುರಕ್ಷಿತವಾಗಿರಿ — ಸಹಾಯ ಬರುತ್ತಿದೆ.

ತುರ್ತು ಸಹಾಯಕ್ಕಾಗಿ 112 ಗೆ ಕರೆ ಮಾಡಬಹುದು.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosTriggered', 'TAMIL', '🚨 SOS எச்சரிக்கை அனுப்பப்பட்டது. பாதுகாப்பாக இருங்கள் — உதவி வருகிறது.

உடனடி அவசர உதவிக்கு 112 ஐ அழைக்கலாம்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosTriggered', 'TELUGU', '🚨 SOS అలర్ట్ పంపబడింది. సురక్షితంగా ఉండండి — సహాయం వస్తోంది.

తక్షణ ఎమర్జెన్సీ సహాయం కోసం 112 కి కూడా కాల్ చేయవచ్చు.', NULL, now(), now()),

  -- sosFailed
  (gen_random_uuid()::text, 'wa_bot_sosFailed', 'ENGLISH', 'Could not trigger SOS: {{0}}

Please call 112 directly for emergency help.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosFailed', 'HINDI', 'SOS नहीं भेज सके: {{0}}

कृपया आपातकालीन मदद के लिए सीधे 112 पर कॉल करें।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosFailed', 'GUJARATI', 'SOS મોકલી શકાયો નથી: {{0}}

કૃપા કરીને ઇમરજન્સી મદદ માટે સીધો 112 પર કૉલ કરો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosFailed', 'KANNADA', 'SOS ಕಳುಹಿಸಲು ಸಾಧ್ಯವಾಗಲಿಲ್ಲ: {{0}}

ದಯವಿಟ್ಟು ತುರ್ತು ಸಹಾಯಕ್ಕಾಗಿ ನೇರವಾಗಿ 112 ಗೆ ಕರೆ ಮಾಡಿ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosFailed', 'TAMIL', 'SOS அனுப்ப இயலவில்லை: {{0}}

தயவுசெய்து அவசர உதவிக்கு நேரடியாக 112 ஐ அழைக்கவும்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sosFailed', 'TELUGU', 'SOS పంపలేకపోయాము: {{0}}

దయచేసి ఎమర్జెన్సీ సహాయం కోసం నేరుగా 112 కి కాల్ చేయండి.', NULL, now(), now()),

  -- markSafeButton
  (gen_random_uuid()::text, 'wa_bot_markSafeButton', 'ENGLISH', '✅ Mark as Safe', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markSafeButton', 'HINDI', '✅ सुरक्षित चिह्नित करें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markSafeButton', 'GUJARATI', '✅ સુરક્ષિત ચિહ્નિત કરો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markSafeButton', 'KANNADA', '✅ ಸುರಕ್ಷಿತ ಎಂದು ಗುರುತಿಸಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markSafeButton', 'TAMIL', '✅ பாதுகாப்பானது எனக் குறிக்கவும்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markSafeButton', 'TELUGU', '✅ సురక్షితం అని గుర్తించండి', NULL, now(), now()),

  -- markSafeConfirm
  (gen_random_uuid()::text, 'wa_bot_markSafeConfirm', 'ENGLISH', 'Are you sure you want to mark your ride as safe? This will cancel the SOS alert.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markSafeConfirm', 'HINDI', 'क्या आप अपनी राइड को सुरक्षित चिह्नित करना चाहते हैं? इससे SOS अलर्ट रद्द हो जाएगा।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markSafeConfirm', 'GUJARATI', 'શું તમે તમારી રાઈડને સુરક્ષિત ચિહ્નિત કરવા માગો છો? આ SOS અલર્ટ રદ કરશે.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markSafeConfirm', 'KANNADA', 'ನಿಮ್ಮ ರೈಡ್ ಅನ್ನು ಸುರಕ್ಷಿತ ಎಂದು ಗುರುತಿಸಲು ಖಚಿತವಾಗಿದ್ದೀರಾ? ಇದು SOS ಎಚ್ಚರಿಕೆಯನ್ನು ರದ್ದು ಮಾಡುತ್ತದೆ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markSafeConfirm', 'TAMIL', 'உங்கள் சவாரியை பாதுகாப்பானது எனக் குறிக்க விரும்புகிறீர்களா? இது SOS எச்சரிக்கையை ரத்து செய்யும்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markSafeConfirm', 'TELUGU', 'మీ రైడ్‌ను సురక్షితం అని గుర్తించాలనుకుంటున్నారా? ఇది SOS అలర్ట్‌ను రద్దు చేస్తుంది.', NULL, now(), now()),

  -- yesMarkSafe
  (gen_random_uuid()::text, 'wa_bot_yesMarkSafe', 'ENGLISH', '✅ Yes, I am safe', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_yesMarkSafe', 'HINDI', '✅ हाँ, मैं सुरक्षित हूँ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_yesMarkSafe', 'GUJARATI', '✅ હા, હું સુરક્ષિત છું', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_yesMarkSafe', 'KANNADA', '✅ ಹೌದು, ನಾನು ಸುರಕ್ಷಿತ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_yesMarkSafe', 'TAMIL', '✅ ஆம், நான் பாதுகாப்பாக இருக்கிறேன்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_yesMarkSafe', 'TELUGU', '✅ అవును, నేను సురక్షితం', NULL, now(), now()),

  -- markedSafe
  (gen_random_uuid()::text, 'wa_bot_markedSafe', 'ENGLISH', '✅ Your ride has been marked as safe. The SOS alert has been cancelled.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markedSafe', 'HINDI', '✅ आपकी राइड सुरक्षित चिह्नित कर दी गई है। SOS अलर्ट रद्द हो गया है।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markedSafe', 'GUJARATI', '✅ તમારી રાઈડ સુરક્ષિત ચિહ્નિત કરાઈ છે. SOS અલર્ટ રદ થયો છે.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markedSafe', 'KANNADA', '✅ ನಿಮ್ಮ ರೈಡ್ ಸುರಕ್ಷಿತ ಎಂದು ಗುರುತಿಸಲಾಗಿದೆ. SOS ಎಚ್ಚರಿಕೆ ರದ್ದಾಗಿದೆ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markedSafe', 'TAMIL', '✅ உங்கள் சவாரி பாதுகாப்பானது எனக் குறிக்கப்பட்டது. SOS எச்சரிக்கை ரத்து செய்யப்பட்டது.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markedSafe', 'TELUGU', '✅ మీ రైడ్ సురక్షితం అని గుర్తించబడింది. SOS అలర్ట్ రద్దు చేయబడింది.', NULL, now(), now()),

  -- markSafeFailed
  (gen_random_uuid()::text, 'wa_bot_markSafeFailed', 'ENGLISH', 'Could not mark as safe: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markSafeFailed', 'HINDI', 'सुरक्षित चिह्नित नहीं कर सके: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markSafeFailed', 'GUJARATI', 'સુરક્ષિત ચિહ્નિત કરી શકાયું નથી: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markSafeFailed', 'KANNADA', 'ಸುರಕ್ಷಿತ ಎಂದು ಗುರುತಿಸಲು ಸಾಧ್ಯವಾಗಲಿಲ್ಲ: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markSafeFailed', 'TAMIL', 'பாதுகாப்பானது எனக் குறிக்க இயலவில்லை: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_markSafeFailed', 'TELUGU', 'సురక్షితం అని గుర్తించలేకపోయాము: {{0}}', NULL, now(), now()),

  -- flexiSharePrompt
  (gen_random_uuid()::text, 'wa_bot_flexiSharePrompt', 'ENGLISH', 'Where should the driver pick you up? 📍', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiSharePrompt', 'HINDI', 'ड्राइवर आपको कहाँ से लेगा? 📍', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiSharePrompt', 'GUJARATI', 'ડ્રાઈવર તમને ક્યાંથી લેશે? 📍', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiSharePrompt', 'KANNADA', 'ಚಾಲಕ ನಿಮ್ಮನ್ನು ಎಲ್ಲಿಂದ ಕರೆದೊಯ್ಯಬೇಕು? 📍', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiSharePrompt', 'TAMIL', 'டிரைவர் உங்களை எங்கிருந்து அழைத்துச் செல்ல வேண்டும்? 📍', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiSharePrompt', 'TELUGU', 'డ్రైవర్ మిమ్మల్ని ఎక్కడ నుండి తీసుకెళ్లాలి? 📍', NULL, now(), now()),

  -- flexiConfirmPickup
  (gen_random_uuid()::text, 'wa_bot_flexiConfirmPickup', 'ENGLISH', '📍 Your location: near *{{0}}*.

Shall we go ahead?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiConfirmPickup', 'HINDI', '📍 आपकी लोकेशन: *{{0}}* के पास।

आगे बढ़ें?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiConfirmPickup', 'GUJARATI', '📍 તમારું લોકેશન: *{{0}}* પાસે.

આગળ વધીએ?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiConfirmPickup', 'KANNADA', '📍 ನಿಮ್ಮ ಸ್ಥಳ: *{{0}}* ಹತ್ತಿರ.

ಮುಂದುವರಿಯೋಣವೇ?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiConfirmPickup', 'TAMIL', '📍 உங்கள் இருப்பிடம்: *{{0}}* அருகில்.

தொடரலாமா?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiConfirmPickup', 'TELUGU', '📍 మీ లొకేషన్: *{{0}}* దగ్గర.

కొనసాగించాలా?', NULL, now(), now()),

  -- flexiConfirmSavedPlace
  (gen_random_uuid()::text, 'wa_bot_flexiConfirmSavedPlace', 'ENGLISH', '📍 You shared a saved place:
 *{{0}}*.
Shall we go ahead?.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiConfirmSavedPlace', 'HINDI', '📍 आपने एक सेव की गई जगह भेजी:
 *{{0}}*।
आगे बढ़ें?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiConfirmSavedPlace', 'GUJARATI', '📍 તમે એક સેવ કરેલી જગ્યા મોકલી:
 *{{0}}*.
આગળ વધીએ?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiConfirmSavedPlace', 'KANNADA', '📍 ನೀವು ಉಳಿಸಿದ ಸ್ಥಳವನ್ನು ಕಳುಹಿಸಿದ್ದೀರಿ:
 *{{0}}*.
ಮುಂದುವರಿಯೋಣವೇ?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiConfirmSavedPlace', 'TAMIL', '📍 நீங்கள் சேமித்த இடத்தை அனுப்பியுள்ளீர்கள்:
 *{{0}}*.
தொடரலாமா?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiConfirmSavedPlace', 'TELUGU', '📍 మీరు సేవ్ చేసిన స్థలాన్ని పంపారు:
 *{{0}}*.
కొనసాగించాలా?', NULL, now(), now()),

  -- pickupConfirmButton
  (gen_random_uuid()::text, 'wa_bot_pickupConfirmButton', 'ENGLISH', '✅ Confirm pickup', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_pickupConfirmButton', 'HINDI', '✅ पिकअप कन्फर्म करें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_pickupConfirmButton', 'GUJARATI', '✅ પિકઅપ કન્ફર્મ કરો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_pickupConfirmButton', 'KANNADA', '✅ ಪಿಕಪ್ ಖಚಿತಪಡಿಸಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_pickupConfirmButton', 'TAMIL', '✅ பிக்அப்பை உறுதிப்படுத்து', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_pickupConfirmButton', 'TELUGU', '✅ పికప్ నిర్ధారించండి', NULL, now(), now()),

  -- pickupAdjustButton
  (gen_random_uuid()::text, 'wa_bot_pickupAdjustButton', 'ENGLISH', '✏️ Change location', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_pickupAdjustButton', 'HINDI', '✏️ जगह बदलें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_pickupAdjustButton', 'GUJARATI', '✏️ જગ્યા બદલો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_pickupAdjustButton', 'KANNADA', '✏️ ಸ್ಥಳ ಬದಲಿಸಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_pickupAdjustButton', 'TAMIL', '✏️ இடத்தை மாற்று', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_pickupAdjustButton', 'TELUGU', '✏️ స్థలం మార్చు', NULL, now(), now()),

  -- flexiFinding
  (gen_random_uuid()::text, 'wa_bot_flexiFinding', 'ENGLISH', '🛺 Finding an auto near you…', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFinding', 'HINDI', '🛺 आपके पास ऑटो ढूँढ रहे हैं…', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFinding', 'GUJARATI', '🛺 તમારી નજીક ઓટો શોધી રહ્યા છીએ…', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFinding', 'KANNADA', '🛺 ನಿಮ್ಮ ಹತ್ತಿರ ಆಟೋ ಹುಡುಕುತ್ತಿದ್ದೇವೆ…', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFinding', 'TAMIL', '🛺 உங்களுக்கு அருகில் ஆட்டோ தேடுகிறோம்…', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFinding', 'TELUGU', '🛺 మీ దగ్గర ఆటో వెతుకుతున్నాము…', NULL, now(), now()),

  -- flexiStillFinding
  (gen_random_uuid()::text, 'wa_bot_flexiStillFinding', 'ENGLISH', '⏳ Still finding an auto near you… ({{0}}s)

Send "cancel" to stop.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiStillFinding', 'HINDI', '⏳ अभी भी आपके पास ऑटो ढूँढ रहे हैं… ({{0}} सेकंड)

रोकने के लिए "cancel" भेजें।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiStillFinding', 'GUJARATI', '⏳ હજુ પણ તમારી નજીક ઓટો શોધી રહ્યા છીએ… ({{0}} સેકંડ)

રોકવા માટે "cancel" મોકલો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiStillFinding', 'KANNADA', '⏳ ಇನ್ನೂ ನಿಮ್ಮ ಹತ್ತಿರ ಆಟೋ ಹುಡುಕುತ್ತಿದ್ದೇವೆ… ({{0}} ಸೆ)

ನಿಲ್ಲಿಸಲು "cancel" ಕಳುಹಿಸಿ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiStillFinding', 'TAMIL', '⏳ இன்னும் உங்களுக்கு அருகில் ஆட்டோ தேடுகிறோம்… ({{0}} வி)

நிறுத்த "cancel" அனுப்பவும்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiStillFinding', 'TELUGU', '⏳ ఇంకా మీ దగ్గర ఆటో వెతుకుతున్నాము… ({{0}} సె)

ఆపడానికి "cancel" పంపండి.', NULL, now(), now()),

  -- flexiCancelSearch
  (gen_random_uuid()::text, 'wa_bot_flexiCancelSearch', 'ENGLISH', '❌ Cancel search', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiCancelSearch', 'HINDI', '❌ खोज रोकें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiCancelSearch', 'GUJARATI', '❌ શોધ રોકો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiCancelSearch', 'KANNADA', '❌ ಹುಡುಕಾಟ ನಿಲ್ಲಿಸಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiCancelSearch', 'TAMIL', '❌ தேடலை நிறுத்து', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiCancelSearch', 'TELUGU', '❌ శోధన ఆపు', NULL, now(), now()),

  -- flexiFoundDriver
  (gen_random_uuid()::text, 'wa_bot_flexiFoundDriver', 'ENGLISH', '🛺 *{{0}}* is on the way.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFoundDriver', 'HINDI', '🛺 *{{0}}* आ रहे हैं।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFoundDriver', 'GUJARATI', '🛺 *{{0}}* આવી રહ્યા છે.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFoundDriver', 'KANNADA', '🛺 *{{0}}* ಬರುತ್ತಿದ್ದಾರೆ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFoundDriver', 'TAMIL', '🛺 *{{0}}* வந்து கொண்டிருக்கிறார்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFoundDriver', 'TELUGU', '🛺 *{{0}}* వస్తున్నారు.', NULL, now(), now()),

  -- flexiDriverMeta
  (gen_random_uuid()::text, 'wa_bot_flexiDriverMeta', 'ENGLISH', '⭐ {{0}} · {{1}} min away', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiDriverMeta', 'HINDI', '⭐ {{0}} · {{1}} मिनट दूर', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiDriverMeta', 'GUJARATI', '⭐ {{0}} · {{1}} મિનિટ દૂર', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiDriverMeta', 'KANNADA', '⭐ {{0}} · {{1}} ನಿಮಿಷ ದೂರ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiDriverMeta', 'TAMIL', '⭐ {{0}} · {{1}} நிமிடம் தொலைவில்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiDriverMeta', 'TELUGU', '⭐ {{0}} · {{1}} నిమిషాల దూరంలో', NULL, now(), now()),

  -- flexiOtpShare
  (gen_random_uuid()::text, 'wa_bot_flexiOtpShare', 'ENGLISH', '🔑 Start OTP: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiOtpShare', 'HINDI', '🔑 स्टार्ट OTP: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiOtpShare', 'GUJARATI', '🔑 સ્ટાર્ટ OTP: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiOtpShare', 'KANNADA', '🔑 ಆರಂಭ OTP: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiOtpShare', 'TAMIL', '🔑 தொடக்க OTP: *{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiOtpShare', 'TELUGU', '🔑 స్టార్ట్ OTP: *{{0}}*', NULL, now(), now()),

  -- flexiCallDriver
  (gen_random_uuid()::text, 'wa_bot_flexiCallDriver', 'ENGLISH', '📞 Call driver: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiCallDriver', 'HINDI', '📞 ड्राइवर को कॉल करें: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiCallDriver', 'GUJARATI', '📞 ડ્રાઈવરને કૉલ કરો: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiCallDriver', 'KANNADA', '📞 ಚಾಲಕರಿಗೆ ಕರೆ ಮಾಡಿ: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiCallDriver', 'TAMIL', '📞 டிரைவரை அழைக்கவும்: {{0}}', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiCallDriver', 'TELUGU', '📞 డ్రైవర్‌కు కాల్ చేయండి: {{0}}', NULL, now(), now()),

  -- flexiSafetyNote
  (gen_random_uuid()::text, 'wa_bot_flexiSafetyNote', 'ENGLISH', 'Confirm your destination with the driver.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiSafetyNote', 'HINDI', 'ड्राइवर के साथ अपना गंतव्य पक्का कर लें।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiSafetyNote', 'GUJARATI', 'ડ્રાઈવર સાથે તમારું ગંતવ્ય કન્ફર્મ કરો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiSafetyNote', 'KANNADA', 'ಚಾಲಕರೊಂದಿಗೆ ನಿಮ್ಮ ಗಮ್ಯಸ್ಥಾನವನ್ನು ಖಚಿತಪಡಿಸಿ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiSafetyNote', 'TAMIL', 'டிரைவரிடம் உங்கள் சேருமிடத்தை உறுதிப்படுத்துங்கள்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiSafetyNote', 'TELUGU', 'డ్రైవర్‌తో మీ గమ్యస్థానాన్ని నిర్ధారించుకోండి.', NULL, now(), now()),

  -- flexiNoAuto
  (gen_random_uuid()::text, 'wa_bot_flexiNoAuto', 'ENGLISH', '😔 No auto available near you right now. Please try again.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiNoAuto', 'HINDI', '😔 अभी आपके पास कोई ऑटो उपलब्ध नहीं है। कृपया फिर कोशिश करें।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiNoAuto', 'GUJARATI', '😔 અત્યારે તમારી નજીક કોઈ ઓટો ઉપલબ્ધ નથી. કૃપા કરીને ફરી પ્રયાસ કરો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiNoAuto', 'KANNADA', '😔 ಸದ್ಯಕ್ಕೆ ನಿಮ್ಮ ಹತ್ತಿರ ಯಾವುದೇ ಆಟೋ ಲಭ್ಯವಿಲ್ಲ. ದಯವಿಟ್ಟು ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiNoAuto', 'TAMIL', '😔 இப்போது உங்களுக்கு அருகில் ஆட்டோ எதுவும் இல்லை. மீண்டும் முயற்சிக்கவும்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiNoAuto', 'TELUGU', '😔 ప్రస్తుతం మీ దగ్గర ఆటో అందుబాటులో లేదు. దయచేసి మళ్లీ ప్రయత్నించండి.', NULL, now(), now()),

  -- flexiTryAgain
  (gen_random_uuid()::text, 'wa_bot_flexiTryAgain', 'ENGLISH', '🔁 Try again', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiTryAgain', 'HINDI', '🔁 फिर कोशिश करें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiTryAgain', 'GUJARATI', '🔁 ફરી પ્રયાસ કરો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiTryAgain', 'KANNADA', '🔁 ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiTryAgain', 'TAMIL', '🔁 மீண்டும் முயற்சிக்கவும்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiTryAgain', 'TELUGU', '🔁 మళ్లీ ప్రయత్నించండి', NULL, now(), now()),

  -- flexiOutOfArea
  (gen_random_uuid()::text, 'wa_bot_flexiOutOfArea', 'ENGLISH', '📍 That location looks outside our service area.

Namma Yatri autos currently run in *{{0}}*. Try a pickup there, or check back soon.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiOutOfArea', 'HINDI', '📍 यह जगह हमारी सेवा क्षेत्र से बाहर लगती है।

Namma Yatri ऑटो फ़िलहाल *{{0}}* में उपलब्ध हैं। वहाँ से पिकअप आज़माएँ, या कुछ समय बाद देखें।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiOutOfArea', 'GUJARATI', '📍 આ સ્થળ અમારા સેવા વિસ્તારની બહાર લાગે છે.

Namma Yatri ઓટો હાલમાં *{{0}}* માં ઉપલબ્ધ છે. ત્યાંથી પિકઅપ અજમાવો, અથવા થોડા સમય પછી ફરી તપાસો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiOutOfArea', 'KANNADA', '📍 ಈ ಸ್ಥಳ ನಮ್ಮ ಸೇವಾ ಪ್ರದೇಶದ ಹೊರಗಿದೆ.

Namma Yatri ಆಟೋಗಳು ಸದ್ಯ *{{0}}* ನಲ್ಲಿ ಲಭ್ಯವಿವೆ. ಅಲ್ಲಿಂದ ಪಿಕಪ್ ಪ್ರಯತ್ನಿಸಿ, ಅಥವಾ ಸ್ವಲ್ಪ ಸಮಯದ ನಂತರ ನೋಡಿ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiOutOfArea', 'TAMIL', '📍 இந்த இடம் எங்கள் சேவைப் பகுதிக்கு வெளியே உள்ளது.

Namma Yatri ஆட்டோக்கள் தற்போது *{{0}}* இல் இயங்குகின்றன. அங்கிருந்து பிக்அப் முயற்சிக்கவும், அல்லது சிறிது நேரம் கழித்து பார்க்கவும்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiOutOfArea', 'TELUGU', '📍 ఈ ప్రదేశం మా సేవా ప్రాంతం వెలుపల ఉంది.

Namma Yatri ఆటోలు ప్రస్తుతం *{{0}}* లో అందుబాటులో ఉన్నాయి. అక్కడి నుండి పికప్ ప్రయత్నించండి, లేదా కొద్దిసేపటి తర్వాత చూడండి.', NULL, now(), now()),

  -- flexiRideStarted
  (gen_random_uuid()::text, 'wa_bot_flexiRideStarted', 'ENGLISH', '🚦 Ride started! Enjoy the ride.

Reached your destination? Tap *End ride* below', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideStarted', 'HINDI', '🚦 राइड शुरू हो गई! राइड का आनंद लें।

अपनी मंज़िल पर पहुँच गए? नीचे *राइड समाप्त करें* पर टैप करें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideStarted', 'GUJARATI', '🚦 રાઈડ શરૂ થઈ! રાઈડનો આનંદ માણો.

તમારા ગંતવ્ય પર પહોંચી ગયા? નીચે *રાઈડ સમાપ્ત કરો* દબાવો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideStarted', 'KANNADA', '🚦 ರೈಡ್ ಪ್ರಾರಂಭವಾಗಿದೆ! ಪ್ರಯಾಣವನ್ನು ಆನಂದಿಸಿ.

ನಿಮ್ಮ ಗಮ್ಯಸ್ಥಾನ ತಲುಪಿದಿರಾ? ಕೆಳಗಿನ *ರೈಡ್ ಮುಗಿಸಿ* ಒತ್ತಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideStarted', 'TAMIL', '🚦 சவாரி தொடங்கியது! சவாரியை ரசியுங்கள்.

உங்கள் சேருமிடத்தை அடைந்துவிட்டீர்களா? கீழே *சவாரியை முடி* என்பதை அழுத்தவும்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideStarted', 'TELUGU', '🚦 రైడ్ ప్రారంభమైంది! ప్రయాణాన్ని ఆస్వాదించండి.

మీ గమ్యస్థానానికి చేరుకున్నారా? క్రింద *రైడ్ ముగించండి* నొక్కండి', NULL, now(), now()),

  -- flexiFareFinal
  (gen_random_uuid()::text, 'wa_bot_flexiFareFinal_noKm', 'ENGLISH', '💰 Total fare: *₹{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareFinal_noKm', 'HINDI', '💰 कुल किराया: *₹{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareFinal_noKm', 'GUJARATI', '💰 કુલ ભાડું: *₹{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareFinal_noKm', 'KANNADA', '💰 ಒಟ್ಟು ಶುಲ್ಕ: *₹{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareFinal_noKm', 'TAMIL', '💰 மொத்த கட்டணம்: *₹{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareFinal_noKm', 'TELUGU', '💰 మొత్తం ఛార్జీ: *₹{{0}}*', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareFinal_withKm', 'ENGLISH', '💰 Total fare: *₹{{0}}* · {{1}} km', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareFinal_withKm', 'HINDI', '💰 कुल किराया: *₹{{0}}* · {{1}} किमी', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareFinal_withKm', 'GUJARATI', '💰 કુલ ભાડું: *₹{{0}}* · {{1}} કિમી', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareFinal_withKm', 'KANNADA', '💰 ಒಟ್ಟು ಶುಲ್ಕ: *₹{{0}}* · {{1}} ಕಿಮೀ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareFinal_withKm', 'TAMIL', '💰 மொத்த கட்டணம்: *₹{{0}}* · {{1}} கிமீ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareFinal_withKm', 'TELUGU', '💰 మొత్తం ఛార్జీ: *₹{{0}}* · {{1}} కిమీ', NULL, now(), now()),

  -- flexiFareUnavailable
  (gen_random_uuid()::text, 'wa_bot_flexiFareUnavailable', 'ENGLISH', '💰 Your fare will be confirmed shortly.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareUnavailable', 'HINDI', '💰 आपका किराया जल्द ही पक्का हो जाएगा।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareUnavailable', 'GUJARATI', '💰 તમારું ભાડું થોડી વારમાં કન્ફર્મ થશે.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareUnavailable', 'KANNADA', '💰 ನಿಮ್ಮ ಶುಲ್ಕ ಶೀಘ್ರದಲ್ಲೇ ಖಚಿತವಾಗುತ್ತದೆ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareUnavailable', 'TAMIL', '💰 உங்கள் கட்டணம் விரைவில் உறுதிப்படுத்தப்படும்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareUnavailable', 'TELUGU', '💰 మీ ఛార్జీ త్వరలో నిర్ధారించబడుతుంది.', NULL, now(), now()),

  -- flexiRideEnded
  (gen_random_uuid()::text, 'wa_bot_flexiRideEnded', 'ENGLISH', '🎉 Ride complete!

{{0}}

🙏 Thank you for riding with Namma Yatri.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideEnded', 'HINDI', '🎉 राइड पूरी हुई!

{{0}}

🙏 Namma Yatri के साथ सफर करने के लिए धन्यवाद।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideEnded', 'GUJARATI', '🎉 રાઈડ પૂર્ણ થઈ!

{{0}}

🙏 Namma Yatri સાથે મુસાફરી કરવા બદલ આભાર.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideEnded', 'KANNADA', '🎉 ರೈಡ್ ಪೂರ್ಣಗೊಂಡಿದೆ!

{{0}}

🙏 Namma Yatri ಜೊತೆ ಪ್ರಯಾಣಿಸಿದ್ದಕ್ಕೆ ಧನ್ಯವಾದಗಳು.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideEnded', 'TAMIL', '🎉 சவாரி முடிந்தது!

{{0}}

🙏 Namma Yatri உடன் பயணித்ததற்கு நன்றி.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideEnded', 'TELUGU', '🎉 రైడ్ పూర్తయింది!

{{0}}

🙏 Namma Yatri తో ప్రయాణించినందుకు ధన్యవాదాలు.', NULL, now(), now()),

  -- flexiRideCancelled
  (gen_random_uuid()::text, 'wa_bot_flexiRideCancelled', 'ENGLISH', '❌ Your ride was cancelled.

Need to go somewhere? Book another auto anytime.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideCancelled', 'HINDI', '❌ आपकी राइड रद्द हो गई।

कहीं जाना है? कभी भी दूसरा ऑटो बुक करें।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideCancelled', 'GUJARATI', '❌ તમારી રાઈડ રદ થઈ ગઈ.

ક્યાંક જવું છે? ગમે ત્યારે બીજો ઓટો બુક કરો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideCancelled', 'KANNADA', '❌ ನಿಮ್ಮ ರೈಡ್ ರದ್ದಾಗಿದೆ.

ಎಲ್ಲಿಗಾದರೂ ಹೋಗಬೇಕೇ? ಯಾವಾಗ ಬೇಕಾದರೂ ಇನ್ನೊಂದು ಆಟೋ ಬುಕ್ ಮಾಡಿ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideCancelled', 'TAMIL', '❌ உங்கள் சவாரி ரத்து செய்யப்பட்டது.

எங்காவது செல்ல வேண்டுமா? எப்போது வேண்டுமானாலும் மற்றொரு ஆட்டோ புக் செய்யுங்கள்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideCancelled', 'TELUGU', '❌ మీ రైడ్ రద్దు చేయబడింది.

ఎక్కడికైనా వెళ్లాలా? ఎప్పుడైనా మరో ఆటో బుక్ చేయండి.', NULL, now(), now()),

  -- flexiBookAnother
  (gen_random_uuid()::text, 'wa_bot_flexiBookAnother', 'ENGLISH', '🛺 Book another', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiBookAnother', 'HINDI', '🛺 दूसरा बुक करें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiBookAnother', 'GUJARATI', '🛺 બીજો બુક કરો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiBookAnother', 'KANNADA', '🛺 ಇನ್ನೊಂದು ಬುಕ್ ಮಾಡಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiBookAnother', 'TAMIL', '🛺 மற்றொன்று புக் செய்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiBookAnother', 'TELUGU', '🛺 మరొకటి బుక్ చేయండి', NULL, now(), now()),

  -- flexiEndRideButton
  (gen_random_uuid()::text, 'wa_bot_flexiEndRideButton', 'ENGLISH', '🏁 End ride', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndRideButton', 'HINDI', '🏁 राइड समाप्त करें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndRideButton', 'GUJARATI', '🏁 રાઈડ સમાપ્ત કરો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndRideButton', 'KANNADA', '🏁 ರೈಡ್ ಮುಗಿಸಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndRideButton', 'TAMIL', '🏁 சவாரியை முடி', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndRideButton', 'TELUGU', '🏁 రైడ్ ముగించండి', NULL, now(), now()),

  -- flexiEndOtpShare
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpShare', 'ENGLISH', '🏁 End OTP: *{{0}}*

Share this with your driver when you reach your destination.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpShare', 'HINDI', '🏁 समाप्ति OTP: *{{0}}*

जब आप अपनी मंज़िल पर पहुँच जाएँ तो यह अपने ड्राइवर को बताएँ।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpShare', 'GUJARATI', '🏁 સમાપ્તિ OTP: *{{0}}*

જ્યારે તમે તમારા ગંતવ્ય પર પહોંચો ત્યારે આ તમારા ડ્રાઈવરને આપો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpShare', 'KANNADA', '🏁 ಮುಕ್ತಾಯ OTP: *{{0}}*

ನೀವು ನಿಮ್ಮ ಗಮ್ಯಸ್ಥಾನ ತಲುಪಿದಾಗ ಇದನ್ನು ನಿಮ್ಮ ಚಾಲಕರಿಗೆ ತಿಳಿಸಿ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpShare', 'TAMIL', '🏁 முடிவு OTP: *{{0}}*

உங்கள் சேருமிடத்தை அடையும்போது இதை உங்கள் டிரைவரிடம் சொல்லுங்கள்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpShare', 'TELUGU', '🏁 ముగింపు OTP: *{{0}}*

మీరు మీ గమ్యస్థానానికి చేరుకున్నప్పుడు దీన్ని మీ డ్రైవర్‌కు చెప్పండి.', NULL, now(), now()),

  -- flexiEndOtpNotReady
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpNotReady', 'ENGLISH', '⏳ Your ride hasn''t started yet. You''ll get the end OTP once you''re on your way.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpNotReady', 'HINDI', '⏳ आपकी राइड अभी शुरू नहीं हुई है। जैसे ही आप रास्ते पर होंगे, आपको समाप्ति OTP मिल जाएगा।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpNotReady', 'GUJARATI', '⏳ તમારી રાઈડ હજુ શરૂ થઈ નથી. જેવા તમે રસ્તે હશો કે તરત તમને સમાપ્તિ OTP મળી જશે.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpNotReady', 'KANNADA', '⏳ ನಿಮ್ಮ ರೈಡ್ ಇನ್ನೂ ಪ್ರಾರಂಭವಾಗಿಲ್ಲ. ನೀವು ದಾರಿಯಲ್ಲಿ ಹೊರಟ ಕೂಡಲೇ ಮುಕ್ತಾಯ OTP ಸಿಗುತ್ತದೆ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpNotReady', 'TAMIL', '⏳ உங்கள் சவாரி இன்னும் தொடங்கவில்லை. நீங்கள் வழியில் சென்றவுடன் முடிவு OTP கிடைக்கும்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpNotReady', 'TELUGU', '⏳ మీ రైడ్ ఇంకా ప్రారంభం కాలేదు. మీరు బయలుదేరిన వెంటనే మీకు ముగింపు OTP వస్తుంది.', NULL, now(), now()),

  -- flexiEndOtpFetchError
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpFetchError', 'ENGLISH', '⚠️ Couldn''t fetch your ride just now. Tap *End ride* again in a moment.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpFetchError', 'HINDI', '⚠️ अभी आपकी राइड नहीं मिल पाई। थोड़ी देर में फिर से *राइड समाप्त करें* पर टैप करें।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpFetchError', 'GUJARATI', '⚠️ અત્યારે તમારી રાઈડ મળી શકી નહીં. થોડી વારમાં ફરીથી *રાઈડ સમાપ્ત કરો* પર ટૅપ કરો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpFetchError', 'KANNADA', '⚠️ ಸದ್ಯಕ್ಕೆ ನಿಮ್ಮ ರೈಡ್ ಪಡೆಯಲಾಗಲಿಲ್ಲ. ಸ್ವಲ್ಪ ಸಮಯದ ನಂತರ ಮತ್ತೆ *ರೈಡ್ ಮುಗಿಸಿ* ಒತ್ತಿ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpFetchError', 'TAMIL', '⚠️ இப்போது உங்கள் சவாரியைப் பெற முடியவில்லை. சிறிது நேரத்தில் மீண்டும் *சவாரியை முடி* என்பதைத் தட்டவும்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiEndOtpFetchError', 'TELUGU', '⚠️ ప్రస్తుతం మీ రైడ్‌ను పొందలేకపోయాం. కొద్దిసేపటి తర్వాత మళ్లీ *రైడ్ ముగించండి* నొక్కండి.', NULL, now(), now()),

  -- flexiRideAlreadyEnded
  (gen_random_uuid()::text, 'wa_bot_flexiRideAlreadyEnded', 'ENGLISH', '✅ This ride has already ended.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideAlreadyEnded', 'HINDI', '✅ यह राइड पहले ही समाप्त हो चुकी है।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideAlreadyEnded', 'GUJARATI', '✅ આ રાઈડ પહેલેથી સમાપ્ત થઈ ગઈ છે.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideAlreadyEnded', 'KANNADA', '✅ ಈ ರೈಡ್ ಈಗಾಗಲೇ ಮುಗಿದಿದೆ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideAlreadyEnded', 'TAMIL', '✅ இந்த சவாரி ஏற்கனவே முடிந்துவிட்டது.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiRideAlreadyEnded', 'TELUGU', '✅ ఈ రైడ్ ఇప్పటికే ముగిసింది.', NULL, now(), now()),

  -- moreButton
  (gen_random_uuid()::text, 'wa_bot_moreButton', 'ENGLISH', '⚙️ More options', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreButton', 'HINDI', '⚙️ और विकल्प', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreButton', 'GUJARATI', '⚙️ વધુ વિકલ્પો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreButton', 'KANNADA', '⚙️ ಇನ್ನಷ್ಟು ಆಯ್ಕೆಗಳು', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreButton', 'TAMIL', '⚙️ மேலும் விருப்பங்கள்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreButton', 'TELUGU', '⚙️ మరిన్ని ఎంపికలు', NULL, now(), now()),

  -- moreTitle
  (gen_random_uuid()::text, 'wa_bot_moreTitle', 'ENGLISH', 'What would you like to do?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreTitle', 'HINDI', 'आप क्या करना चाहेंगे?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreTitle', 'GUJARATI', 'તમે શું કરવા માગો છો?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreTitle', 'KANNADA', 'ನೀವು ಏನು ಮಾಡಲು ಬಯಸುತ್ತೀರಿ?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreTitle', 'TAMIL', 'நீங்கள் என்ன செய்ய விரும்புகிறீர்கள்?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreTitle', 'TELUGU', 'మీరు ఏమి చేయాలనుకుంటున్నారు?', NULL, now(), now()),

  -- howItWorks
  (gen_random_uuid()::text, 'wa_bot_howItWorks', 'ENGLISH', '❓ How it works', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_howItWorks', 'HINDI', '❓ यह कैसे काम करता है', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_howItWorks', 'GUJARATI', '❓ કેવી રીતે કામ કરે', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_howItWorks', 'KANNADA', '❓ ಹೇಗೆ ಕೆಲಸ ಮಾಡುತ್ತದೆ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_howItWorks', 'TAMIL', '❓ எப்படி வேலை செய்கிறது', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_howItWorks', 'TELUGU', '❓ ఇది ఎలా పనిచేస్తుంది', NULL, now(), now()),

  -- contactSupport
  (gen_random_uuid()::text, 'wa_bot_contactSupport', 'ENGLISH', '💬 Support', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_contactSupport', 'HINDI', '💬 सहायता', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_contactSupport', 'GUJARATI', '💬 સહાય', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_contactSupport', 'KANNADA', '💬 ಬೆಂಬಲ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_contactSupport', 'TAMIL', '💬 ஆதரவு', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_contactSupport', 'TELUGU', '💬 మద్దతు', NULL, now(), now()),

  -- howItWorksText
  (gen_random_uuid()::text, 'wa_bot_howItWorksText', 'ENGLISH', '📹 *How Namma Yatri works*

1️⃣ Tap *Book a Ride*
2️⃣ Share your pickup location 📍
3️⃣ We find you a nearby auto
4️⃣ Meet your driver, share the OTP, and go!

_(Intro video coming soon.)_', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_howItWorksText', 'HINDI', '📹 *Namma Yatri कैसे काम करता है*

1️⃣ *राइड बुक करें* पर टैप करें
2️⃣ अपना पिकअप स्थान साझा करें 📍
3️⃣ हम आपके पास एक ऑटो ढूँढते हैं
4️⃣ अपने ड्राइवर से मिलें, OTP बताएँ, और चलें!

_(परिचय वीडियो जल्द आ रहा है।)_', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_howItWorksText', 'GUJARATI', '📹 *Namma Yatri કેવી રીતે કામ કરે છે*

1️⃣ *રાઈડ બુક કરો* દબાવો
2️⃣ તમારું પિકઅપ સ્થાન શેર કરો 📍
3️⃣ અમે તમારી નજીક એક ઓટો શોધીએ છીએ
4️⃣ તમારા ડ્રાઈવરને મળો, OTP આપો, અને ચાલો!

_(પરિચય વીડિયો ટૂંક સમયમાં આવી રહ્યો છે.)_', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_howItWorksText', 'KANNADA', '📹 *Namma Yatri ಹೇಗೆ ಕೆಲಸ ಮಾಡುತ್ತದೆ*

1️⃣ *ರೈಡ್ ಬುಕ್ ಮಾಡಿ* ಒತ್ತಿ
2️⃣ ನಿಮ್ಮ ಪಿಕಪ್ ಸ್ಥಳ ಹಂಚಿಕೊಳ್ಳಿ 📍
3️⃣ ನಾವು ನಿಮ್ಮ ಹತ್ತಿರ ಆಟೋ ಹುಡುಕುತ್ತೇವೆ
4️⃣ ನಿಮ್ಮ ಚಾಲಕರನ್ನು ಭೇಟಿಯಾಗಿ, OTP ಹೇಳಿ, ಹೊರಡಿ!

_(ಪರಿಚಯ ವೀಡಿಯೊ ಶೀಘ್ರದಲ್ಲೇ ಬರಲಿದೆ.)_', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_howItWorksText', 'TAMIL', '📹 *Namma Yatri எப்படி வேலை செய்கிறது*

1️⃣ *சவாரி புக் செய்* என்பதை அழுத்தவும்
2️⃣ உங்கள் பிக்அப் இடத்தைப் பகிரவும் 📍
3️⃣ உங்களுக்கு அருகில் ஒரு ஆட்டோவைக் கண்டுபிடிக்கிறோம்
4️⃣ உங்கள் டிரைவரைச் சந்தித்து, OTP ஐ சொல்லி, புறப்படுங்கள்!

_(அறிமுக வீடியோ விரைவில் வரும்.)_', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_howItWorksText', 'TELUGU', '📹 *Namma Yatri ఎలా పనిచేస్తుంది*

1️⃣ *రైడ్ బుక్ చేయండి* నొక్కండి
2️⃣ మీ పికప్ ప్రదేశం షేర్ చేయండి 📍
3️⃣ మేము మీ దగ్గర ఆటోను కనుగొంటాము
4️⃣ మీ డ్రైవర్‌ను కలవండి, OTP చెప్పండి, బయలుదేరండి!

_(పరిచయ వీడియో త్వరలో వస్తోంది.)_', NULL, now(), now()),

  -- howItWorksCaption
  (gen_random_uuid()::text, 'wa_bot_howItWorksCaption', 'ENGLISH', 'How to book an auto on Namma Yatri 🛺', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_howItWorksCaption', 'HINDI', 'Namma Yatri पर ऑटो कैसे बुक करें 🛺', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_howItWorksCaption', 'GUJARATI', 'Namma Yatri પર ઓટો કેવી રીતે બુક કરવો 🛺', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_howItWorksCaption', 'KANNADA', 'Namma Yatri ನಲ್ಲಿ ಆಟೋ ಹೇಗೆ ಬುಕ್ ಮಾಡುವುದು 🛺', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_howItWorksCaption', 'TAMIL', 'Namma Yatri இல் ஆட்டோ எப்படி புக் செய்வது 🛺', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_howItWorksCaption', 'TELUGU', 'Namma Yatri లో ఆటో ఎలా బుక్ చేయాలి 🛺', NULL, now(), now()),

  -- supportMessage
  (gen_random_uuid()::text, 'wa_bot_supportMessage', 'ENGLISH', '💬 Need help?

Call us: {{0}}

We''re here to help. 🙏', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_supportMessage', 'HINDI', '💬 मदद चाहिए?

हमें कॉल करें: {{0}}

हम आपकी मदद के लिए यहाँ हैं। 🙏', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_supportMessage', 'GUJARATI', '💬 મદદ જોઈએ છે?

અમને કૉલ કરો: {{0}}

અમે તમારી મદદ માટે અહીં છીએ. 🙏', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_supportMessage', 'KANNADA', '💬 ಸಹಾಯ ಬೇಕೇ?

ನಮಗೆ ಕರೆ ಮಾಡಿ: {{0}}

ನಿಮ್ಮ ಸಹಾಯಕ್ಕಾಗಿ ನಾವು ಇಲ್ಲಿದ್ದೇವೆ. 🙏', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_supportMessage', 'TAMIL', '💬 உதவி வேண்டுமா?

எங்களை அழைக்கவும்: {{0}}

உங்களுக்கு உதவ நாங்கள் இங்கே இருக்கிறோம். 🙏', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_supportMessage', 'TELUGU', '💬 సహాయం కావాలా?

మాకు కాల్ చేయండి: {{0}}

మీకు సహాయం చేయడానికి మేము ఇక్కడ ఉన్నాము. 🙏', NULL, now(), now()),

  -- rideTypePrompt
  (gen_random_uuid()::text, 'wa_bot_rideTypePrompt', 'ENGLISH', 'How would you like to travel?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideTypePrompt', 'HINDI', 'आप कैसे यात्रा करना चाहेंगे?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideTypePrompt', 'GUJARATI', 'તમે કેવી રીતે મુસાફરી કરવા માગો છો?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideTypePrompt', 'KANNADA', 'ನೀವು ಹೇಗೆ ಪ್ರಯಾಣಿಸಲು ಬಯಸುತ್ತೀರಿ?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideTypePrompt', 'TAMIL', 'நீங்கள் எப்படி பயணிக்க விரும்புகிறீர்கள்?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideTypePrompt', 'TELUGU', 'మీరు ఎలా ప్రయాణించాలనుకుంటున్నారు?', NULL, now(), now()),

  -- rideTypeFlexi
  (gen_random_uuid()::text, 'wa_bot_rideTypeFlexi', 'ENGLISH', '🛺 Quick Ride', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideTypeFlexi', 'HINDI', '🛺 झटपट राइड', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideTypeFlexi', 'GUJARATI', '🛺 ઝડપી રાઈડ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideTypeFlexi', 'KANNADA', '🛺 ತ್ವರಿತ ರೈಡ್', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideTypeFlexi', 'TAMIL', '🛺 விரைவு சவாரி', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideTypeFlexi', 'TELUGU', '🛺 త్వరిత రైడ్', NULL, now(), now()),

  -- rideTypeRegular
  (gen_random_uuid()::text, 'wa_bot_rideTypeRegular', 'ENGLISH', '🚗 Ride with destination', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideTypeRegular', 'HINDI', '🚗 मंज़िल वाली राइड', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideTypeRegular', 'GUJARATI', '🚗 ગંતવ્ય રાઈડ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideTypeRegular', 'KANNADA', '🚗 ಗಮ್ಯದ ರೈಡ್', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideTypeRegular', 'TAMIL', '🚗 சேருமிட சவாரி', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideTypeRegular', 'TELUGU', '🚗 గమ్యం రైడ్', NULL, now(), now()),

  -- rideStartedSimple
  (gen_random_uuid()::text, 'wa_bot_rideStartedSimple', 'ENGLISH', '🚦 Your ride has started. Enjoy the trip!', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideStartedSimple', 'HINDI', '🚦 आपकी राइड शुरू हो गई है। सफर का आनंद लें!', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideStartedSimple', 'GUJARATI', '🚦 તમારી રાઈડ શરૂ થઈ ગઈ છે. મુસાફરીનો આનંદ માણો!', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideStartedSimple', 'KANNADA', '🚦 ನಿಮ್ಮ ರೈಡ್ ಪ್ರಾರಂಭವಾಗಿದೆ. ಪ್ರಯಾಣವನ್ನು ಆನಂದಿಸಿ!', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideStartedSimple', 'TAMIL', '🚦 உங்கள் சவாரி தொடங்கியது. பயணத்தை ரசியுங்கள்!', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_rideStartedSimple', 'TELUGU', '🚦 మీ రైడ్ ప్రారంభమైంది. ప్రయాణాన్ని ఆస్వాదించండి!', NULL, now(), now()),

  -- regularDropPrompt
  (gen_random_uuid()::text, 'wa_bot_regularDropPrompt', 'ENGLISH', 'Where are you going? 📍

Share your drop location, or type the address.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularDropPrompt', 'HINDI', 'आपको कहाँ जाना है? 📍

अपनी ड्रॉप लोकेशन साझा करें, या पता टाइप करें।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularDropPrompt', 'GUJARATI', 'તમારે ક્યાં જવું છે? 📍

તમારું ડ્રોપ લોકેશન શેર કરો, અથવા સરનામું ટાઈપ કરો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularDropPrompt', 'KANNADA', 'ನೀವು ಎಲ್ಲಿಗೆ ಹೋಗುತ್ತಿದ್ದೀರಿ? 📍

ನಿಮ್ಮ ಡ್ರಾಪ್ ಸ್ಥಳ ಹಂಚಿಕೊಳ್ಳಿ, ಅಥವಾ ವಿಳಾಸ ಟೈಪ್ ಮಾಡಿ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularDropPrompt', 'TAMIL', 'நீங்கள் எங்கு செல்கிறீர்கள்? 📍

உங்கள் டிராப் இடத்தைப் பகிரவும், அல்லது முகவரியைத் தட்டச்சு செய்யவும்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularDropPrompt', 'TELUGU', 'మీరు ఎక్కడికి వెళ్తున్నారు? 📍

మీ డ్రాప్ లొకేషన్ షేర్ చేయండి, లేదా చిరునామా టైప్ చేయండి.', NULL, now(), now()),

  -- regularSelectDrop
  (gen_random_uuid()::text, 'wa_bot_regularSelectDrop', 'ENGLISH', 'Which one? Pick your drop:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularSelectDrop', 'HINDI', 'कौन सी? अपनी ड्रॉप चुनें:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularSelectDrop', 'GUJARATI', 'કઈ? તમારું ડ્રોપ પસંદ કરો:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularSelectDrop', 'KANNADA', 'ಯಾವುದು? ನಿಮ್ಮ ಡ್ರಾಪ್ ಆಯ್ಕೆ ಮಾಡಿ:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularSelectDrop', 'TAMIL', 'எது? உங்கள் டிராப்பைத் தேர்ந்தெடுக்கவும்:', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularSelectDrop', 'TELUGU', 'ఏది? మీ డ్రాప్ ఎంచుకోండి:', NULL, now(), now()),

  -- regularFareConfirm
  (gen_random_uuid()::text, 'wa_bot_regularFareConfirm', 'ENGLISH', '🛺 Auto to *{{1}}*
💰 Approx *₹{{0}}*

Shall I book it?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularFareConfirm', 'HINDI', '🛺 *{{1}}* तक ऑटो
💰 लगभग *₹{{0}}*

बुक कर दूँ?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularFareConfirm', 'GUJARATI', '🛺 *{{1}}* સુધી ઓટો
💰 આશરે *₹{{0}}*

બુક કરી દઉં?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularFareConfirm', 'KANNADA', '🛺 *{{1}}* ವರೆಗೆ ಆಟೋ
💰 ಸುಮಾರು *₹{{0}}*

ಬುಕ್ ಮಾಡಲೇ?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularFareConfirm', 'TAMIL', '🛺 *{{1}}* வரை ஆட்டோ
💰 சுமார் *₹{{0}}*

புக் செய்யவா?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularFareConfirm', 'TELUGU', '🛺 *{{1}}* వరకు ఆటో
💰 సుమారు *₹{{0}}*

బుక్ చేయనా?', NULL, now(), now()),

  -- regularConfirmButton
  (gen_random_uuid()::text, 'wa_bot_regularConfirmButton', 'ENGLISH', '✅ Book auto', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularConfirmButton', 'HINDI', '✅ ऑटो बुक करें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularConfirmButton', 'GUJARATI', '✅ ઓટો બુક કરો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularConfirmButton', 'KANNADA', '✅ ಆಟೋ ಬುಕ್ ಮಾಡಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularConfirmButton', 'TAMIL', '✅ ஆட்டோ புக் செய்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularConfirmButton', 'TELUGU', '✅ ఆటో బుక్ చేయండి', NULL, now(), now()),

  -- regularChangeDropButton
  (gen_random_uuid()::text, 'wa_bot_regularChangeDropButton', 'ENGLISH', '✏️ Change drop', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularChangeDropButton', 'HINDI', '✏️ ड्रॉप बदलें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularChangeDropButton', 'GUJARATI', '✏️ ડ્રોપ બદલો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularChangeDropButton', 'KANNADA', '✏️ ಡ್ರಾಪ್ ಬದಲಿಸಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularChangeDropButton', 'TAMIL', '✏️ டிராப் மாற்று', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularChangeDropButton', 'TELUGU', '✏️ డ్రాప్ మార్చు', NULL, now(), now()),

  -- regularSearching
  (gen_random_uuid()::text, 'wa_bot_regularSearching', 'ENGLISH', '🛺 Getting your fare…', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularSearching', 'HINDI', '🛺 आपका किराया ला रहे हैं…', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularSearching', 'GUJARATI', '🛺 તમારું ભાડું લાવી રહ્યા છીએ…', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularSearching', 'KANNADA', '🛺 ನಿಮ್ಮ ಶುಲ್ಕ ಪಡೆಯುತ್ತಿದ್ದೇವೆ…', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularSearching', 'TAMIL', '🛺 உங்கள் கட்டணத்தைப் பெறுகிறோம்…', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularSearching', 'TELUGU', '🛺 మీ ఛార్జీని తెస్తున్నాము…', NULL, now(), now()),

  -- regularBooking
  (gen_random_uuid()::text, 'wa_bot_regularBooking', 'ENGLISH', '🛺 Booking your auto…', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularBooking', 'HINDI', '🛺 आपका ऑटो बुक कर रहे हैं…', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularBooking', 'GUJARATI', '🛺 તમારો ઓટો બુક કરી રહ્યા છીએ…', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularBooking', 'KANNADA', '🛺 ನಿಮ್ಮ ಆಟೋ ಬುಕ್ ಮಾಡುತ್ತಿದ್ದೇವೆ…', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularBooking', 'TAMIL', '🛺 உங்கள் ஆட்டோவை புக் செய்கிறோம்…', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularBooking', 'TELUGU', '🛺 మీ ఆటోను బుక్ చేస్తున్నాము…', NULL, now(), now()),

  -- somethingWentWrong
  (gen_random_uuid()::text, 'wa_bot_somethingWentWrong', 'ENGLISH', 'Something went wrong. Send "book" to start over.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_somethingWentWrong', 'HINDI', 'कुछ गड़बड़ हो गई। दोबारा शुरू करने के लिए "book" भेजें।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_somethingWentWrong', 'GUJARATI', 'કંઈક ખોટું થયું. ફરીથી શરૂ કરવા "book" મોકલો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_somethingWentWrong', 'KANNADA', 'ಏನೋ ತಪ್ಪಾಗಿದೆ. ಮತ್ತೆ ಪ್ರಾರಂಭಿಸಲು "book" ಕಳುಹಿಸಿ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_somethingWentWrong', 'TAMIL', 'ஏதோ தவறு ஏற்பட்டது. மீண்டும் தொடங்க "book" அனுப்பவும்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_somethingWentWrong', 'TELUGU', 'ఏదో తప్పు జరిగింది. మళ్ళీ ప్రారంభించడానికి "book" పంపండి.', NULL, now(), now()),

  -- sessionExpired
  (gen_random_uuid()::text, 'wa_bot_sessionExpired', 'ENGLISH', 'Session expired. Send "book" to re-authenticate.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sessionExpired', 'HINDI', 'सत्र समाप्त हो गया। पुनः प्रमाणित करने के लिए "book" भेजें।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sessionExpired', 'GUJARATI', 'સત્ર સમાપ્ત થયું. ફરીથી પ્રમાણિત થવા "book" મોકલો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sessionExpired', 'KANNADA', 'ಸೆಶನ್ ಅವಧಿ ಮುಗಿದಿದೆ. ಮರು-ದೃಢೀಕರಿಸಲು "book" ಕಳುಹಿಸಿ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sessionExpired', 'TAMIL', 'அமர்வு காலாவதியானது. மீண்டும் அங்கீகரிக்க "book" அனுப்பவும்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_sessionExpired', 'TELUGU', 'సెషన్ గడువు ముగిసింది. మళ్ళీ ప్రామాణీకరించడానికి "book" పంపండి.', NULL, now(), now()),

  -- error
  (gen_random_uuid()::text, 'wa_bot_error', 'ENGLISH', 'Error: {{0}}
Send "cancel" to start over.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_error', 'HINDI', 'त्रुटि: {{0}}
दोबारा शुरू करने के लिए "cancel" भेजें।', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_error', 'GUJARATI', 'ભૂલ: {{0}}
ફરીથી શરૂ કરવા "cancel" મોકલો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_error', 'KANNADA', 'ದೋಷ: {{0}}
ಮತ್ತೆ ಪ್ರಾರಂಭಿಸಲು "cancel" ಕಳುಹಿಸಿ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_error', 'TAMIL', 'பிழை: {{0}}
மீண்டும் தொடங்க "cancel" அனுப்பவும்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_error', 'TELUGU', 'లోపం: {{0}}
మళ్ళీ ప్రారంభించడానికి "cancel" పంపండి.', NULL, now(), now()),

  -- Gap-fill: Phase 1 (0047) only ever seeded English/Hindi/Kannada for its 6
  -- pilot fields (welcome, chooseLanguage, setupFailed, languageUpdated,
  -- flexiFareRate, flexiArrived) -- Gujarati/Tamil/Telugu were missing for
  -- those 6, unnoticed while resolveField still fell back to the static
  -- tables. Now that resolveField is a hard, no-fallback DB read, that gap
  -- broke replies for Gujarati/Tamil/Telugu users outright (confirmed live).
  -- welcome
  (gen_random_uuid()::text, 'wa_bot_welcome', 'GUJARATI', '🙏 નમસ્તે! હું તમારો Namma Yatri સહાયક છું

ઓટો બુક કરવા તૈયાર છો?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_welcome', 'TAMIL', '🙏 வணக்கம்! நான் உங்கள் Namma Yatri உதவியாளர்

ஆட்டோ புக் செய்ய தயாரா?', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_welcome', 'TELUGU', '🙏 నమస్కారం! నేను మీ Namma Yatri అసిస్టెంట్

ఆటో బుక్ చేయడానికి సిద్ధంగా ఉన్నారా?', NULL, now(), now()),

  -- chooseLanguage
  (gen_random_uuid()::text, 'wa_bot_chooseLanguage', 'GUJARATI', '🌐 ભાષા', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_chooseLanguage', 'TAMIL', '🌐 மொழி', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_chooseLanguage', 'TELUGU', '🌐 భాష', NULL, now(), now()),

  -- setupFailed ({{0}} = error text)
  (gen_random_uuid()::text, 'wa_bot_setupFailed', 'GUJARATI', 'સેટઅપ નિષ્ફળ: {{0}}
ફરીથી પ્રયાસ કરવા "book" મોકલો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_setupFailed', 'TAMIL', 'அமைப்பு தோல்வியடைந்தது: {{0}}
மீண்டும் முயற்சிக்க "book" அனுப்பவும்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_setupFailed', 'TELUGU', 'సెటప్ విఫలమైంది: {{0}}
మళ్ళీ ప్రయత్నించడానికి "book" పంపండి.', NULL, now(), now()),

  -- languageUpdated ({{0}} = new language display name)
  (gen_random_uuid()::text, 'wa_bot_languageUpdated', 'GUJARATI', '✅ ભાષા *{{0}}* માં બદલાઈ ગઈ.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_languageUpdated', 'TAMIL', '✅ மொழி *{{0}}* ஆக மாற்றப்பட்டது.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_languageUpdated', 'TELUGU', '✅ భాష *{{0}}*కి మార్చబడింది.', NULL, now(), now()),

  -- flexiFareRate ({{0}} = base fare, {{1}} = per-km fare)
  (gen_random_uuid()::text, 'wa_bot_flexiFareRate', 'GUJARATI', '🛺 મીટર ઓટો · ₹{{0}} + ₹{{1}}/કિમીથી', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareRate', 'TAMIL', '🛺 மீட்டர் ஆட்டோ · ₹{{0}} + ₹{{1}}/கிமீ முதல்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiFareRate', 'TELUGU', '🛺 మీటర్ ఆటో · ₹{{0}} + ₹{{1}}/కిమీ నుండి', NULL, now(), now()),

  -- flexiArrived, no-OTP branch
  (gen_random_uuid()::text, 'wa_bot_flexiArrived_noOtp', 'GUJARATI', '🛺 તમારો ઓટો આવી ગયો છે!
કૃપા કરીને પિકઅપ પોઈન્ટ પર તમારા ડ્રાઈવરને મળો.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiArrived_noOtp', 'TAMIL', '🛺 உங்கள் ஆட்டோ வந்துவிட்டது!
பிக்அப் இடத்தில் உங்கள் டிரைவரைச் சந்திக்கவும்.', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiArrived_noOtp', 'TELUGU', '🛺 మీ ఆటో వచ్చేసింది!
దయచేసి పికప్ పాయింట్ వద్ద మీ డ్రైవర్‌ను కలవండి.', NULL, now(), now()),

  -- flexiArrived, with-OTP branch ({{0}} = OTP)
  (gen_random_uuid()::text, 'wa_bot_flexiArrived_withOtp', 'GUJARATI', '🛺 તમારો ઓટો આવી ગયો છે!
રાઈડ શરૂ કરવા ડ્રાઈવરને OTP આપો.

🔑 OTP: *{{0}}* ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiArrived_withOtp', 'TAMIL', '🛺 உங்கள் ஆட்டோ வந்துவிட்டது!
சவாரியைத் தொடங்க டிரைவரிடம் OTP ஐ சொல்லுங்கள்.

🔑 OTP: *{{0}}* ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_flexiArrived_withOtp', 'TELUGU', '🛺 మీ ఆటో వచ్చేసింది!
రైడ్ ప్రారంభించడానికి డ్రైవర్‌కు OTP చెప్పండి.

🔑 OTP: *{{0}}* ', NULL, now(), now())
ON CONFLICT (message_key, language) WHERE merchant_operating_city_id IS NULL DO NOTHING;
