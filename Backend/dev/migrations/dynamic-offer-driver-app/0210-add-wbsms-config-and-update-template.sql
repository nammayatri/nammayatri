INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, service_name, config_json)
SELECT m.id, 'Sms_WBSms',
  json_build_object(
      'passkey','0.1.0|0|jN+1Tmxs1/eXVPN0ikeDdICF3KZLWQous8NAVXRXi356kZVaQbGP3mIyHxZq4qsOa4osqmLHMA=='
    , 'templSendOtp','1407168570168978854'
    , 'templAltNo','1407168570138137956'
    , 'templWelcome','1407168570176997771'
    , 'url','https://barta.wb.gov.in'
  )
FROM atlas_driver_offer_bpp.merchant m;

UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET sms_providers_priority_list='{"WBSms"}'
WHERE merchant_id = 'favorit0-0000-0000-0000-00000favorit'; --change the merchant_id to yatri sathi merchant id

UPDATE atlas_driver_offer_bpp.merchant_message
SET message = '{#otp#} is your OTP for login to Yatri Sathi App. {#hash#}. DITE GoWB'
WHERE message_key ='SEND_OTP' AND merchant_id = 'favorit0-0000-0000-0000-00000favorit'; -- change the merchant_id to yatri sathi merchant id

UPDATE atlas_driver_offer_bpp.merchant_message
SET message = '{#otp#} is your OTP for adding alternate number in Yatri Sathi App. {#hash#}. DITE GoWB'
WHERE message_key ='ALTERNATE_NUMBER_OTP' AND merchant_id = 'favorit0-0000-0000-0000-00000favorit'; -- change the merchant_id to yatri sathi merchant id