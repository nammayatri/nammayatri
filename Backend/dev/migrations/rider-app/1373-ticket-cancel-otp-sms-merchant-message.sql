WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'CANCEL_FRFS_TICKET_OTP_PARTNERORG', '{#otp#} is your OTP to cancel your metro ticket.
.  -Namma Yatri', T1.id ,'NMYTRI'
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id,sender_header)
  (SELECT * FROM MerchantMessages);
