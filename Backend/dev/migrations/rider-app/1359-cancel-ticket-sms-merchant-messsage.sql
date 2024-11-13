-- NOTE: Don't run below query in master or prod

WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'METRO_TICKET_BOOKING_CANCELLED', 'Your metro {#TICKET_PLURAL#} cancelled {#URL#} Click the link to view and manage your booking. Thank you for choosing Namma Yatri!', T1.id ,'NMYTRI'
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id,sender_header)
  (SELECT * FROM MerchantMessages);
