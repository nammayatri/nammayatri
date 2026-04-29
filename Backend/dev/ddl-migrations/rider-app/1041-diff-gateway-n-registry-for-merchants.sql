

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
