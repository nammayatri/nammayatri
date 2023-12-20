CREATE TABLE atlas_app.system_configs (
    id VARCHAR(255) PRIMARY KEY,
    config_value TEXT
);


INSERT INTO atlas_app.system_configs (id, config_value)
VALUES ('kv_configs', '{"enableKVForWriteAlso":[{"nameOfTable":"Table1","percentEnable":100}],"enableKVForRead":["Table2"]}');
