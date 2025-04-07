UPDATE atlas_app.merchant_service_config
SET config_json = jsonb_set(config_json::jsonb, '{entityId}', '"0.1.0|0|4i4XQoRBe1PAMU5rTQah50ZrH+tktmU0pF2h1JBfG23YAP+0kuypyY4L8aj/qrf8VC/t52Ok/MSwUUeco4/hgMIZ"') --change this value acc to Prod Encryption
WHERE service_name = 'Sms_GupShup';