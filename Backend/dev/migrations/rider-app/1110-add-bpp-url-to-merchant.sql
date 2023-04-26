ALTER TABLE atlas_app.merchant ADD COLUMN bpp_base_url text;

UPDATE atlas_app.merchant SET bpp_base_url = 'http://localhost:8016/beckn';
--'http://localhost:8015/v1'

ALTER TABLE atlas_app.merchant ALTER COLUMN bpp_base_url SET NOT NULL;

--http://localhost:8016/beckn