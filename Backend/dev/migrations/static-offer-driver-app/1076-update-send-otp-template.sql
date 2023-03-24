UPDATE atlas_transporter.merchant_message
SET message = '{#otp#} is your OTP for login to Namma Yatri App. {#hash#}'
WHERE message_key ='SEND_OTP'