-- NOTE : Disable "person, search_request, app_installs" table in KV till the expiry time (6hrs)
ALTER TABLE atlas_app.person ALTER COLUMN client_version TYPE text;
ALTER TABLE atlas_app.person ALTER COLUMN bundle_version TYPE text;

ALTER TABLE atlas_app.search_request ALTER COLUMN bundle_version TYPE text;
ALTER TABLE atlas_app.search_request ALTER COLUMN client_version TYPE text;

ALTER TABLE atlas_app.app_installs ALTER COLUMN app_version TYPE text;
ALTER TABLE atlas_app.app_installs ALTER COLUMN bundle_version TYPE text;