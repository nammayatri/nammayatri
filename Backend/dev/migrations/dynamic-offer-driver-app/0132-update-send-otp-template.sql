UPDATE atlas_driver_offer_bpp.merchant_message
SET message = '{#otp#} is your OTP for login to Namma Yatri App. {#hash#}'
WHERE message_key ='SEND_OTP'