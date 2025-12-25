-- Note: Only for Local , don't run in prod or master.
UPDATE atlas_app.frfs_config
SET
    valid_till_seconds = 120
WHERE
    merchant_operating_city_id = 'namma-yatri-0-0000-0000-00000000city';