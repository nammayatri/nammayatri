-- we are removing the apiKey from the config_json as it is no longer needed
-- already ran in master
-- this query is only for Prod
UPDATE atlas_app.merchant_service_config
SET config_json = (config_json::jsonb - 'apiKey')::json
WHERE service_name = 'Ticket_Kapture';