UPDATE atlas_driver_offer_bpp.system_configs
SET config_value = '{
  "enableKVForRead": ["table1", "table2", "table3"],
  "enableKVForWriteAlso": [
    { "nameOfTable": "table1", "percentEnable": 100 }
  ],
  "disableForKV": ["table1", "table2", "table3"],
  "kvTablesTtl": {
    "table1": 3600,
    "table2": 7200,
    "table3": 14400
  },
  "useCAC": ["table4", "table5"],
  "useCACForFrontend": true,
  "readFromMasterDb": ["table6", "table7"]
}'
WHERE id ='kv_configs';