ALTER TABLE atlas_app.search_request ADD COLUMN client_sdk_version text ;
ALTER TABLE atlas_app.search_request ADD COLUMN client_os_version text ;
ALTER TABLE atlas_app.search_request ADD COLUMN client_os_type text ;
ALTER TABLE atlas_app.search_request ADD COLUMN client_config_version text ;
ALTER TABLE atlas_app.search_request ADD COLUMN client_bundle_version text ;
ALTER TABLE atlas_app.search_request ADD COLUMN backend_config_version text ;
ALTER TABLE atlas_app.search_request ADD COLUMN backend_app_version text ;

ALTER TABLE atlas_app.booking ADD COLUMN client_sdk_version text ;
ALTER TABLE atlas_app.booking ADD COLUMN client_os_version text ;
ALTER TABLE atlas_app.booking ADD COLUMN client_os_type text ;
ALTER TABLE atlas_app.booking ADD COLUMN client_config_version text ;
ALTER TABLE atlas_app.booking ADD COLUMN client_bundle_version text ;
ALTER TABLE atlas_app.booking ADD COLUMN backend_config_version text ;
ALTER TABLE atlas_app.booking ADD COLUMN backend_app_version text ;

---------------------------------------------------------------------------------
-----------------------------DROP COLUMNS----------------------------------------
---------------------------------------------------------------------------------

UPDATE atlas_app.search_request SET client_bundle_version = bundle_version;
UPDATE atlas_app.search_request SET client_sdk_version = client_version;

ALTER TABLE atlas_app.search_request DROP COLUMN bundle_version;
ALTER TABLE atlas_app.search_request DROP COLUMN client_version;