WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'METRO_TICKET_BOOKING_CANCELLED', 'You have successfully cancelled your metro {#TICKET_PLURAL#} and the refund is in progress. Please visit {#URL#} to view details. Thank you for choosing Namma Yatri! -Namma Yatri', T1.id ,'NMYTRI'
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id,sender_header)
  (SELECT * FROM MerchantMessages);
