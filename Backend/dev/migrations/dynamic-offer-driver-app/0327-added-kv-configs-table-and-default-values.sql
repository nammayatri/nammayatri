CREATE TABLE atlas_driver_offer_bpp.system_configs (
    id VARCHAR(255) PRIMARY KEY,
    config_value TEXT
);

-- just for reference
INSERT INTO atlas_driver_offer_bpp.system_configs (id, config_value)
VALUES ('kv_configs', '{"enableKVForWriteAlso":[{"nameOfTable":"Table1","percentEnable":100}],"enableKVForRead":["Table2"]}');
