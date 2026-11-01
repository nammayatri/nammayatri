-- WhatsApp bot copy: regularFareConfirm now shows the full pickup and drop
-- addresses plus the estimated fare, instead of only the drop's area.
-- {{0}} = fare, {{1}} = drop address, {{2}} = pickup address
-- (WhatsappBot.Flow.Booking.sendRegularFareConfirm). {{1}} stays the drop, as
-- in 0061, so a deploy in either order never shows the drop under "From".
--
-- Non-English copy here is freshly authored -- worth a native-speaker review
-- before this is relied on in production.
UPDATE atlas_app.translations
SET message = CASE language
    WHEN 'ENGLISH' THEN '🛺 *From:* {{2}}
📍 *To:* {{1}}
💰 *Estimated fare:* ₹{{0}}

Shall I book it?'
    WHEN 'HINDI' THEN '🛺 *कहाँ से:* {{2}}
📍 *कहाँ तक:* {{1}}
💰 *अनुमानित किराया:* ₹{{0}}

बुक कर दूँ?'
    WHEN 'GUJARATI' THEN '🛺 *ક્યાંથી:* {{2}}
📍 *ક્યાં સુધી:* {{1}}
💰 *અંદાજિત ભાડું:* ₹{{0}}

બુક કરી દઉં?'
    WHEN 'KANNADA' THEN '🛺 *ಎಲ್ಲಿಂದ:* {{2}}
📍 *ಎಲ್ಲಿಗೆ:* {{1}}
💰 *ಅಂದಾಜು ದರ:* ₹{{0}}

ಬುಕ್ ಮಾಡಲೇ?'
    WHEN 'TAMIL' THEN '🛺 *புறப்படும் இடம்:* {{2}}
📍 *சேருமிடம்:* {{1}}
💰 *மதிப்பிடப்பட்ட கட்டணம்:* ₹{{0}}

புக் செய்யவா?'
    WHEN 'TELUGU' THEN '🛺 *ఎక్కడి నుండి:* {{2}}
📍 *ఎక్కడికి:* {{1}}
💰 *అంచనా ఛార్జీ:* ₹{{0}}

బుక్ చేయనా?'
  END,
  updated_at = now()
WHERE message_key = 'wa_bot_regularFareConfirm'
  AND merchant_operating_city_id IS NULL
  AND language IN ('ENGLISH', 'HINDI', 'GUJARATI', 'KANNADA', 'TAMIL', 'TELUGU');
