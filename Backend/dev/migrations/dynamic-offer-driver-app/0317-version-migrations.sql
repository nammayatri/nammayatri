-- Note : Disable "person" table in KV till the expiry time (6hrs)
ALTER TABLE atlas_driver_offer_bpp.person ALTER COLUMN client_version TYPE text;
ALTER TABLE atlas_driver_offer_bpp.person ALTER COLUMN bundle_version TYPE text;
