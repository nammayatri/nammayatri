INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json)
SELECT m.id, 'Sms_MyValueFirst',
  json_build_object(
      'username','0.1.0|2|nP/5EXjeoVjgH1s9LbAy/VdVByNBlTI6ZfqT5rcXDAo5MNwKeejnLy7CeP5+Hm8vVPWM3qUfmqdsjrjfJQ=='
    , 'password','0.1.0|2|gKVPR0m1qe/6NSNz2EBx6IjxagSagfsL+kbL36htuDYrpQ9F3dFGZ6d6zOuPY1Tk4uYMSMZRJHq6yF4='
    , 'url','http://localhost:4343'
  )
FROM atlas_app.merchant m;

INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json)
SELECT m.id, 'Sms_ExotelSms',
  json_build_object(
      'apiKey','0.1.0|1|DoT32ljUozGmqzLcIPoInLPqrw6rXRquuAuMINhTDV+wU9gI0rtBkCnDh8zz0yuzcFZerBOWPigP2RmTB05ilSybL7MpuA/uPlm6yBYKMEVt8z76nrbx5+a9P81VVLSoR3Bl'
    , 'apiToken','0.1.0|1|VX8BmY9cwsvErnmXjjfrwcOePFK1yzBxTov/9mwnnBzuGIfA4kXVt5MZ7N/q7YEaU9u3w2le6aQthwrFBZjtle8isRS3y7CoEsRJEd3yll3n+oTcGRJr4n1FRzYvqPcU2zlv'
    , 'sid','juspay2m'
    , 'url','api.in.exotel.com'
  )
FROM atlas_app.merchant m;

ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN send_s_m_s character varying(30);

UPDATE atlas_app.merchant_service_usage_config
SET send_s_m_s = 'ExotelSms';