
-- we are removing the apiKey from the config_json as it is no longer needed
-- this query both Master and Prod
UPDATE atlas_app.merchant_service_config
SET config_json = config_json - 'apiKey'
WHERE service_name = 'Ticket_Kapture';

-- this query is only for master
UPDATE atlas_app.merchant_service_config
SET config_json = jsonb_set(
    config_json::jsonb,
    '{auth}',
    to_jsonb('0.1.0|4|pqbiEmDw0gGyJ9zd62C/AEiVxOUvUe2uixvu6neyyhAA1z7Hd7r7hvdBqYVHTw9E5zSDbUy5rIJXtbyClbCQEKxdU5wsSHUNLG8gqxcTQ0alCpYKBFbBkDPpd0O1++MeTfkAKtOHoigl0GjkIX6AmoqyLkgmry1i70t7WOQ='::text)
)
WHERE service_name = 'Ticket_Kapture';
