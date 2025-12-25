-- this query is for master and (for prod we need to update the config_json appEncryptionKey with prod value)
UPDATE atlas_app.merchant_service_config
SET config_json = jsonb_set(
      config_json::jsonb,
      '{appEncryptionKey}',
      to_jsonb('0.1.0|1|Z8uSSnWcJ+3NKEUl+a68B59dKvxHYg3Kca9gohxfYRe2vivbq0xMBN1cPIFkoymL5N18fRao3fI3tBUAIhLSmEM5Fg=='::text)
    )
WHERE service_name = 'Ticket_Kapture';