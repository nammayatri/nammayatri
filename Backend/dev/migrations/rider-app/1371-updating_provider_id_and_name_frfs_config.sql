-- Note: Only for Local , don't run in prod or master.
UPDATE atlas_app.frfs_config
SET
    provider_id = 'triffy-kmrl-rail-metro',
    provider_name = 'Kochi Metro Rail Limited'
WHERE
    merchant_operating_city_id = 'namma-yatri-0-0000-0000-00000000city';