UPDATE atlas_app.merchant_service_config
SET config_json = jsonb_set(config_json::jsonb, '{entityId}', '"0.1.0|0|4i4XQoRBe1PAMU5rTQah50ZrH+tktmU0pF2h1JBfG23YAP+0kuypyY4L8aj/qrf8VC/t52Ok/MSwUUeco4/hgMIZ"') --change this value acc to Prod Encryption
WHERE service_name = 'Sms_GupShup';




--- do not use this for prod only for local testing
UPDATE atlas_app.merchant_service_config
SET config_json = '{
  "userid": "0.1.0|4|1gQihxxPaZdXOmWs4KgosbG3Y1WZkFsQwka6kylaZC0so2P4/hrhdbe79MB+azZSGETKxmMYwMx++WyTcb8JzM4e",
  "password": "0.1.0|4|1gQihxxPaZdXOmWs4KgosbG3Y1WZkFsQwka6kylaZC0so2P4/hrhdbe79MB+azZSGETKxmMYwMx++WyTcb8JzM4e",
  "auth_scheme": "plain",
  "channel": "whatsapp",
  "v": "1.1",
  "url": "https://media.smsgupshup.com/GatewayAPI/rest",
  "format": "json",
  "otp_cfg": {
    "msg_type": "Text",
    "method": "SendMessage",
    "template_id": "6786864",
    "entity_id": "0.1.0|4|1gQihxxPaZdXOmWs4KgosbG3Y1WZkFsQwka6kylaZC0so2P4/hrhdbe79MB+azZSGETKxmMYwMx++WyTcb8JzM4e"
  }
}'::json
WHERE service_name = 'Whatsapp_GupShup';


