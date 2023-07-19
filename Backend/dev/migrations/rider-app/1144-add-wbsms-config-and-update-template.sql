INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json)
SELECT m.id, 'Sms_WBSms',
  json_build_object(
      'passkey','0.1.0|0|jN+1Tmxs1/eXVPN0ikeDdICF3KZLWQous8NAVXRXi356kZVaQbGP3mIyHxZq4qsOa4osqmLHMA=='
    , 'templSendOtp','1407168570168978854'
    , 'templAltNo','1407168570138137956'
    , 'templWelcome','1407168570176997771'
    , 'url','https://barta.wb.gov.in'
  )
FROM atlas_app.merchant m;

UPDATE atlas_app.merchant_service_usage_config
SET sms_providers_priority_list='{"WBSms"}'
WHERE merchant_id = 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51'; --change the merchant_id to yatri sathi merchant id

UPDATE atlas_app.merchant_message
SET message = '{#otp#} is your OTP for login to Yatri Sathi App. {#hash#}. DITE GoWB'
WHERE message_key ='SEND_OTP' AND merchant_id = 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51'; -- change the merchant_id to yatri sathi merchant id