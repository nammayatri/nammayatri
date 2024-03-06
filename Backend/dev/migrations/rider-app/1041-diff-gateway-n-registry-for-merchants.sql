ALTER TABLE
    atlas_app.merchant
ADD
    COLUMN registry_url character varying(255),
ADD
    COLUMN gateway_url character varying(255);

UPDATE
    atlas_app.merchant
SET
    registry_url = 'https://staging.registry.ondc.org',
    gateway_url = 'https://staging.gateway.proteantech.in';

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
