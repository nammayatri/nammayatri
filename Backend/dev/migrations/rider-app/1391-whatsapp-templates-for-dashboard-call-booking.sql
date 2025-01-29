WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'WHATSAPP_CALL_BOOKING_FLOW_DETAILS_MESSAGE',
  'Your ride is confirmed!\n\nHere are your ride details:\n\nDriver Number: {#var1#}\nVehicle Number: {#var2#}\nPrice: {#var3#}\nOTP: {#var4#}\nTracking Link: {#var5#}\nApp Download Link: {#var6#}\n\nHave a smooth ride!',
  '7570578', T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, template_id, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);

WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'WHATSAPP_CALL_BOOKING_REALLOCATED_RIDE_DETAILS_MESSAGE',
  'Your ride has been re-allocated to a new driver.\n\nHere are your ride details:\n\nDriver Number: {#var1#}\nVehicle Number: {#var2#}\nPrice: {#var3#}\nOTP: {#var4#}\nTracking Link: {#var5#}\nApp Download Link: {#var6#}\n\nHave a smooth ride!',
  '7560318', T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, template_id, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);

WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'WHATSAPP_CALL_BOOKING_CANCELLED_RIDE_MESSAGE', 'We apologize for the inconvenience caused.\nUnfortunately, we could not find a suitable ride for you at this time.',
  '7560350', T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, template_id, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);