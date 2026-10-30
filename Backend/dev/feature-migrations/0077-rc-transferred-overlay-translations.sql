-- Translated RC transfer overlay (SharedLogic.RCTransferNotice). 0075 seeds ENGLISH only, and the backend
-- falls back to ENGLISH, so without these rows every non-English driver gets English text in the push.
--
-- Each translated row is copied from the city's ENGLISH row (actions, link, req_body, contact_support_number, ...),
-- so run this after 0076 to carry its OPEN_LINK actions over. 0076 updates every language, so running it
-- later is also fine. Translations are static: nothing is machine-translated at runtime. {#rcNumber#} and
-- "RC" stay in Latin script; the backend fills {#rcNumber#} with the full registration number.
--
-- Idempotent: each INSERT is guarded by NOT EXISTS on (merchant_operating_city_id, overlay_key, language),
-- so re-runs and hand-added rows are left alone.
--
-- Rollback:
--   DELETE FROM atlas_driver_offer_bpp.merchant_overlay
--   WHERE overlay_key = 'RC_TRANSFERRED_TO_ANOTHER_ACCOUNT'
--     AND language IN ('HINDI', 'KANNADA', 'TAMIL', 'TELUGU', 'MALAYALAM', 'BENGALI', 'ODIA', 'FRENCH')
--     AND id = md5(merchant_operating_city_id || '-' || overlay_key || '-' || language);

INSERT INTO atlas_driver_offer_bpp.merchant_overlay
  (id, language, merchant_id, merchant_operating_city_id, overlay_key, udf1, vehicle_category,
   title, description, ok_button_text, cancel_button_text,
   actions, actions2, secondary_actions, secondary_actions2, link, method, req_body, end_point,
   delay, contact_support_number, toast_message, show_push_notification, social_media_links, image_url)
SELECT
  md5(en.merchant_operating_city_id || '-' || en.overlay_key || '-' || t.language),
  t.language,
  en.merchant_id,
  en.merchant_operating_city_id,
  en.overlay_key,
  en.udf1,
  en.vehicle_category,
  t.title,
  t.description,
  t.ok_button_text,
  t.cancel_button_text,
  en.actions,
  en.actions2,
  en.secondary_actions,
  en.secondary_actions2,
  en.link,
  en.method,
  en.req_body,
  en.end_point,
  en.delay,
  en.contact_support_number,
  en.toast_message,
  en.show_push_notification,
  en.social_media_links,
  en.image_url
FROM atlas_driver_offer_bpp.merchant_overlay en
JOIN (VALUES
  ('HINDI',
   'RC दूसरे अकाउंट से लिंक हो गई है',
   'वाहन {#rcNumber#} अब दूसरे अकाउंट से लिंक हो गया है।',
   'वाहन जोड़ें',
   'सपोर्ट को कॉल करें'),
  ('KANNADA',
   'RC ಬೇರೆ ಖಾತೆಗೆ ಲಿಂಕ್ ಆಗಿದೆ',
   'ವಾಹನ {#rcNumber#} ಈಗ ಬೇರೆ ಖಾತೆಗೆ ಲಿಂಕ್ ಆಗಿದೆ.',
   'ವಾಹನ ಸೇರಿಸಿ',
   'ಸಪೋರ್ಟ್‌ಗೆ ಕರೆ ಮಾಡಿ'),
  ('TAMIL',
   'RC வேறொரு கணக்குடன் இணைக்கப்பட்டுள்ளது',
   'வாகனம் {#rcNumber#} இப்போது வேறொரு கணக்குடன் இணைக்கப்பட்டுள்ளது.',
   'வாகனத்தைச் சேர்',
   'உதவி மையத்தை அழைக்கவும்'),
  ('TELUGU',
   'RC మరొక ఖాతాకు లింక్ అయింది',
   'వాహనం {#rcNumber#} ఇప్పుడు మరొక ఖాతాకు లింక్ అయింది.',
   'వాహనాన్ని జోడించండి',
   'సపోర్ట్‌కు కాల్ చేయండి'),
  ('MALAYALAM',
   'RC മറ്റൊരു അക്കൗണ്ടുമായി ബന്ധിപ്പിച്ചു',
   'വാഹനം {#rcNumber#} ഇപ്പോൾ മറ്റൊരു അക്കൗണ്ടുമായി ബന്ധിപ്പിച്ചിരിക്കുന്നു.',
   'വാഹനം ചേർക്കുക',
   'സപ്പോർട്ടിനെ വിളിക്കുക'),
  ('BENGALI',
   'RC অন্য অ্যাকাউন্টের সাথে লিঙ্ক হয়েছে',
   'গাড়ি {#rcNumber#} এখন অন্য একটি অ্যাকাউন্টের সাথে লিঙ্ক করা হয়েছে।',
   'গাড়ি যোগ করুন',
   'সাপোর্টে কল করুন'),
  ('ODIA',
   'RC ଅନ୍ୟ ଏକ ଆକାଉଣ୍ଟ ସହ ଲିଙ୍କ ହୋଇଛି',
   'ଗାଡ଼ି {#rcNumber#} ବର୍ତ୍ତମାନ ଅନ୍ୟ ଏକ ଆକାଉଣ୍ଟ ସହ ଲିଙ୍କ ହୋଇଛି।',
   'ଗାଡ଼ି ଯୋଡ଼ନ୍ତୁ',
   'ସପୋର୍ଟକୁ କଲ୍ କରନ୍ତୁ'),
  ('FRENCH',
   'RC associé à un autre compte',
   'Le véhicule {#rcNumber#} est désormais associé à un autre compte.',
   'Ajouter un véhicule',
   'Appeler le support')
) AS t(language, title, description, ok_button_text, cancel_button_text) ON TRUE
WHERE en.language = 'ENGLISH'
  AND en.overlay_key = 'RC_TRANSFERRED_TO_ANOTHER_ACCOUNT'
  -- the per-city row 0075 seeds; variant rows would collide on the md5 id
  AND en.udf1 IS NULL
  AND en.vehicle_category IS NULL
  AND NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.merchant_overlay mo
    WHERE mo.merchant_operating_city_id = en.merchant_operating_city_id
      AND mo.overlay_key = en.overlay_key
      AND mo.language = t.language
      AND mo.udf1 IS NULL
  );
