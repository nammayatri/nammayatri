-- WhatsApp bot copy: the regular fare confirmation gets a third button,
-- "Change pickup" (WhatsappBot.Flow.Booking.sendRegularFareConfirm), because the
-- regular flow no longer asks "Shall we go ahead?" after the pickup pin.
--
-- Run BEFORE deploying the rider-app build that reads it: a key missing in every
-- language fails the bot's translation load (WhatsappBot.Adapter.Translations).
--
-- Non-English copy here is freshly authored -- worth a native-speaker review
-- before this is relied on in production.
INSERT INTO atlas_app.translations (id, message_key, language, message, merchant_operating_city_id, created_at, updated_at) VALUES
  (gen_random_uuid()::text, 'wa_bot_regularChangePickupButton', 'ENGLISH', '📍 Change pickup', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularChangePickupButton', 'HINDI', '📍 पिकअप बदलें', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularChangePickupButton', 'GUJARATI', '📍 પિકઅપ બદલો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularChangePickupButton', 'KANNADA', '📍 ಪಿಕಪ್ ಬದಲಿಸಿ', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularChangePickupButton', 'TAMIL', '📍 பிக்கப் மாற்று', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_regularChangePickupButton', 'TELUGU', '📍 పికప్ మార్చు', NULL, now(), now())
ON CONFLICT (message_key, language) WHERE merchant_operating_city_id IS NULL DO NOTHING;
