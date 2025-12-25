-- this query is only for master
UPDATE atlas_app.merchant_service_config
SET config_json = jsonb_set(
    config_json::jsonb,
    '{auth}',
    to_jsonb('0.1.0|2|lJI1P6OvckakTCNbgJP0wfucipCGt39sFof8XAXy7zqP+jYTU0Z69jLzIazQNCBSI32z/X5xdENeEL/nXlc8O96xbpGnEKA93vl9/rPCgbCg/tMy/Ot0h80mSLFqbuAf2twQjkZcRWCW1eiWwiNpBLTEJD+K5ta3OcxHGPA='::text)
)
WHERE service_name = 'Ticket_Kapture';
