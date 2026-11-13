-- Runs after config-sync (files here are applied in name order by
-- test-tool/context-api/server.py _apply_local_testing_data), before
-- rider-app is restarted.
--
-- ddl-migrations/rider-app/1028 made atlas_app.merchant.kapture_disposition
-- nullable, but rider-app's Beam type is still Text. A NULL there makes
-- rider-app crash on startup (BeamRowReadError ColumnUnexpectedNull, column 35
-- in Storage.Queries.MerchantExtra.findAll). The column is deprecated (support
-- ticket label), so an empty string is enough to let rider-app start.
UPDATE atlas_app.merchant SET kapture_disposition = '' WHERE kapture_disposition IS NULL;
