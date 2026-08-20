-- Meta_CloudApi's row in merchant_service_config is now dead: its 3 fields
-- (accessToken/baseUrl/apiVersion) moved onto meta_config
-- (dev/feature-migrations/0047-meta-config-access-token-backfill.sql
-- backfilled them; dev/ddl-migrations/rider-app/1562-meta-config-access-token-not-null.sql
-- tightened them to NOT NULL). Nothing reads this row anymore
-- (Tools/Meta.hs.lookupMetaCfg no longer queries merchant_service_config).
DELETE FROM atlas_app.merchant_service_config WHERE service_name = 'Meta_CloudApi';
