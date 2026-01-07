CREATE TABLE atlas_driver_offer_bpp.system_configs (
    id VARCHAR(255) PRIMARY KEY,
    config_value TEXT
);

-- just for reference
INSERT INTO atlas_driver_offer_bpp.system_configs (id, config_value)
VALUES ('kv_configs', '{"enableKVForWriteAlso":[{"nameOfTable":"Table1","percentEnable":100,"redisTtl":18000}],"enableKVForRead":["Table2"], "useCAC":[], "useCACForFrontend":true, "readFromMasterDb":[], "allTablesDisabled": true, "enableSecondaryCloudRead": null, "tablesForSecondaryCloudRead": null}'); -- make sure ttl is in seconds and added to each table


INSERT INTO atlas_driver_offer_bpp.system_configs (id, config_value)
VALUES ('test', '{}');

INSERT INTO atlas_driver_offer_bpp.system_configs (id, config_value)
VALUES ('dev', '{}');
