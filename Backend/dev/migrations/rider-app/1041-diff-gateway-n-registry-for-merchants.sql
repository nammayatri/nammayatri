UPDATE
    atlas_app.merchant
SET
    registry_url = 'http://localhost:8020',
    gateway_url = 'http://localhost:8015/v1';

ALTER TABLE
    atlas_app.merchant
ALTER COLUMN
    registry_url
SET
    NOT NULL;

ALTER TABLE
    atlas_app.merchant
ALTER COLUMN
    gateway_url
SET
    NOT NULL;
