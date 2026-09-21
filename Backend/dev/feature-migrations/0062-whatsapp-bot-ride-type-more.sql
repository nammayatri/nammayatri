-- WhatsApp bot copy: wa_bot_moreRideTypes, the "More" button revealing ride
-- types beyond the first 2 direct slots on the ride-type chooser prompt
-- (WhatsappBot.Flow.Booking.rideTypeButtons) — distinct from
-- wa_bot_moreButton, which opens the settings/help drawer, a different menu.
INSERT INTO atlas_app.translations (id, message_key, language, message, merchant_operating_city_id, created_at, updated_at)
VALUES
  (gen_random_uuid()::text, 'wa_bot_moreRideTypes', 'ENGLISH', '➕ More options', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreRideTypes', 'HINDI', '➕ और विकल्प', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreRideTypes', 'GUJARATI', '➕ વધુ વિકલ્પો', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreRideTypes', 'KANNADA', '➕ ಇನ್ನಷ್ಟು ಆಯ್ಕೆಗಳು', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreRideTypes', 'TAMIL', '➕ மேலும் விருப்பங்கள்', NULL, now(), now()),
  (gen_random_uuid()::text, 'wa_bot_moreRideTypes', 'TELUGU', '➕ మరిన్ని ఎంపికలు', NULL, now(), now())
ON CONFLICT (message_key, language) WHERE merchant_operating_city_id IS NULL DO NOTHING;
