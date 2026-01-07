-- Please be careful while running this query in production.
-- To ensure backward compatibility, keep the existing fields unchanged.
-- Make sure all tables, except those enabled for KV, are included in the 'disableForKV' field.
-- Update the TTL as per the old data.
-- below query is for local testing and should be used as a reference only.
/*
-----------------------------------------------------------------------------------------------------------------------

 To fetch the tables to disable for KV, run the below query:
 Add more tables in the list if they are enabled in kv

SELECT table_name
FROM information_schema.tables
WHERE table_schema = 'atlas_app'
  AND table_type = 'BASE TABLE'
  AND table_name NOT IN ('fare_breakup','registration_token','search_request','person_flow_status','estimate_breakup','quote_breakup','estimate','location','on_search_event','driver_offer','quote','ride','booking_cancellation_reason','booking_location','location_mapping','sos','auto_complete_data','booking','next_billion_data','person','safety_settings','namma_tag','booking_parties_link','search_request_parties_link'
  );

-----------------------------------------------------------------------------------------------------------------------
 Here's the new table object structure:

data Tables = Tables
  { disableForKV :: [Text],
    kvTablesTtl :: HM.HashMap Text Integer,
    useCAC :: [Text],
    useCACForFrontend :: Bool,
    readFromMasterDb :: [Text],
    defaultShardMod :: Int,
    tableShardModRange :: HM.HashMap Text (Int, Int),
    tableRedisKeyPrefix :: HM.HashMap Text Text
  }

*/

UPDATE atlas_app.system_configs
SET config_value = '{
  "enableKVForRead": ["registration_token","search_request","person_flow_status","estimate_breakup","estimate","location","on_search_event","driver_offer","quote","ride","booking_cancellation_reason","booking_location","booking","location_mapping","sos","auto_complete_data","fare_breakup","next_billion_data","quote_breakup","person","safety_settings","namma_tag","booking_parties_link","search_request_parties_link"],
  "enableKVForWriteAlso": [{"nameOfTable":"fare_breakup","percentEnable":100},{"nameOfTable":"registration_token","percentEnable":100},{"nameOfTable":"search_request","percentEnable":100,"redisTtl":2400},{"nameOfTable":"person_flow_status","percentEnable":100},{"nameOfTable":"estimate_breakup","percentEnable":100,"redisTtl":2400},{"nameOfTable":"quote_breakup","percentEnable":100,"redisTtl":2400},{"nameOfTable":"estimate","percentEnable":100,"redisTtl":7200},{"nameOfTable":"location","percentEnable":100,"redisTtl":14400},{"nameOfTable":"on_search_event","percentEnable":100,"redisTtl":2400},{"nameOfTable":"driver_offer","percentEnable":100,"redisTtl":2400},{"nameOfTable":"quote","percentEnable":100,"redisTtl":2400},{"nameOfTable":"ride","percentEnable":100},{"nameOfTable":"booking_cancellation_reason","percentEnable":100,"redisTtl":2400},{"nameOfTable":"booking_location","percentEnable":100},{"nameOfTable":"location_mapping","percentEnable":100,"redisTtl":14400},{"nameOfTable":"sos","percentEnable":100,"redisTtl":2400},{"nameOfTable":"auto_complete_data","percentEnable":100,"redisTtl":2400},{"nameOfTable":"booking","percentEnable":100},{"nameOfTable":"next_billion_data","percentEnable":100,"redisTtl":2400},{"nameOfTable":"person","percentEnable":100},{"nameOfTable":"safety_settings","percentEnable":100},{"nameOfTable":"namma_tag","percentEnable":100},{"nameOfTable":"booking_parties_link","percentEnable":100},{"nameOfTable":"search_request_parties_link","percentEnable":100}],
  "disableForKV": ["issue_config","client","person_stats","special_occasion","disability","disability_translation","system_configs","ticket_service","ticket_booking_service_price_breakup","ticket_service_price","merchant_operating_city","merchant_config","gate_info","app_installs","beckn_request","service_category","seat_management","hot_spot_config","black_list_org","service_people_category","booking_update_request","bpp_details","business_hour","ticket_booking","cancellation_reason","directions_cache_bak","merchant","white_list_org","person_disability","exophone","frfs_quote","issue","value_add_np","geometry","client_person_info","frfs_search","payment_transaction","frfs_ticket_booking","payment_order","merchant_push_notification","merchant_service_usage_config","beckn_config","partner_org_config","frfs_config","ticket_booking_people_category","ticket_booking_service","partner_org_station","ticket_booking_service_category","ticket_place","partner_organization","station","issue_option","issue_report","frfs_recon","frfs_ticket","frfs_ticket_booking_payment","issue_category","ride_related_notification_config","issue_message","rental_slab","call_status","merchant_message","person_default_emergency_number","place_name_cache","saved_location","search_request_location","special_zone_quote","special_location","tag_category_mapping","trip_terms","rider_config", "app_dynamic_logic_rollout","app_dynamic_logic_element"],
  "kvTablesTtl": {
    "search_request":2400,
    "estimate_breakup":2400,
    "quote_breakup":2400,
    "estimate":7200,
    "location":14400,
    "on_search_event":2400,
    "driver_offer":2400,
    "quote":2400,
    "booking_cancellation_reason":2400,
    "location_mapping":14400,
    "sos":2400,
    "auto_complete_data":2400,
    "next_billion_data":2400
  },
  "useCAC": [],
  "useCACForFrontend": false,
  "readFromMasterDb": [],
  "defaultShardMod": 128,
  "tableShardModRange": {},
  "tableRedisKeyPrefix": {},
  "allTablesDisabled": true,
  "enableSecondaryCloudRead": null,
  "tablesForSecondaryCloudRead": null
}'
WHERE id ='kv_configs';