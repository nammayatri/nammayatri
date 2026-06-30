INSERT INTO atlas_driver_offer_bpp.merchant_service_config (
    merchant_id,
    merchant_operating_city_id,
    service_name,
    config_json,
    created_at,
    updated_at
)
SELECT
    merchant_id,
    id,
    'Settlement_Razorpay',
    '{
      "settlementService": "Razorpay",
      "bankCode": "RAZR0001",
      "sourceConfig": {
        "tag": "EmailSourceConfig",
        "contents": {
          "imapHost": "imap.gmail.com",
          "imapPort": 993,
          "username": "imap8182@gmail.com",
          "password": "0.1.0|2|pgxwN/++IAZJ+LiR1rdRqK/KFHgLetNBEL9xy2/ZOGmq8car3i5ekNauxAfX8pGrAmcVjI9GNJt/IvWgOsg9UI+ARg==",
          "folderName": "INBOX",
          "subjectFilter": "Settlement Report",
          "limit": null
        }
      },
      "parserTypeMap": null,
      "useJuspayOrderStatus": null
    }'::jsonb,
    NOW(),
    NOW()
FROM atlas_driver_offer_bpp.merchant_operating_city
WHERE merchant_short_id = 'MSIL_PARTNER' AND city = 'Hyderabad';


-- CCAvenue
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (
    merchant_id,
    merchant_operating_city_id,
    service_name,
    config_json,
    created_at,
    updated_at
)
SELECT
    merchant_id,
    id,
    'Settlement_CCAvenue',
    '{
      "settlementService": "CCAvenue",
      "bankCode": "CCAV0001",
      "sourceConfig": {
        "tag": "EmailSourceConfig",
        "contents": {
          "imapHost": "imap.gmail.com",
          "imapPort": 993,
          "username": "imap8182@gmail.com",
          "password": "0.1.0|2|pgxwN/++IAZJ+LiR1rdRqK/KFHgLetNBEL9xy2/ZOGmq8car3i5ekNauxAfX8pGrAmcVjI9GNJt/IvWgOsg9UI+ARg==",
          "folderName": "INBOX",
          "subjectFilter": "Settlement Report",
          "limit": null
        }
      },
      "parserTypeMap": null,
      "useJuspayOrderStatus": null
    }'::jsonb,
    NOW(),
    NOW()
FROM atlas_driver_offer_bpp.merchant_operating_city
WHERE merchant_short_id = 'MSIL_PARTNER' AND city = 'Hyderabad';

  -- Update existing YesBiz & Billdesk to add bankCode (if config exists)
  UPDATE atlas_driver_offer_bpp.merchant_service_config
  SET config_json = (config_json::jsonb || '{"bankCode": "YESB0001"}'::jsonb)::jsonb,
      updated_at = now()
  WHERE service_name = 'Settlement_YesBiz' AND merchant_operating_city_id IN (select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id = 'MSIL_PARTNER');

  UPDATE atlas_driver_offer_bpp.merchant_service_config
  SET config_json = (config_json::jsonb || '{"bankCode": "BILL0001"}'::jsonb)::jsonb,
      updated_at = now()
  WHERE service_name = 'Settlement_BillDesk' AND merchant_operating_city_id IN (select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id = 'MSIL_PARTNER');
