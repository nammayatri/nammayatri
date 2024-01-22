ALTER TABLE atlas_app.merchant ADD COLUMN frfs_registry_url character varying(255);
ALTER TABLE atlas_app.merchant ADD COLUMN frfs_gateway_url character varying(255);

UPDATE atlas_app.merchant SET
    frfs_registry_url = 'http://localhost:8020',
    frfs_gateway_url = 'http://localhost:8015/v1';

ALTER TABLE atlas_app.merchant ALTER COLUMN frfs_registry_url SET NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN frfs_gateway_url SET NOT NULL;