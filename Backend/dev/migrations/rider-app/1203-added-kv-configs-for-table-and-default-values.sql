CREATE TABLE atlas_app.system_configs (
    id VARCHAR(255) PRIMARY KEY,
    config_value TEXT
);

-- make sure ttl is in seconds and added to each table
INSERT INTO atlas_app.system_configs (id, config_value)
VALUES ('kv_configs', '{"enableKVForWriteAlso":[{"nameOfTable":"Table1","percentEnable":100,"redisTtl":18000}],"enableKVForRead":["Table2"], "useCAC":[], "useCACForFrontend":true, "readFromMasterDb":[], "allTablesDisabled": false, "enableSecondaryCloudRead": null, "tablesForSecondaryCloudRead": null}');
