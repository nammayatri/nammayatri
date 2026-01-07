-- Please be careful while running this query in production.
-- To ensure backward compatibility, keep the existing fields unchanged.
-- Make sure all tables, except those enabled for KV, are included in the 'disableForKV' field.
-- Update the TTL as per the old data.
-- Below query is for local testing and should be used as a reference only.
/*
-----------------------------------------------------------------------------------------------------------------------
 To fetch the tables to disable for KV, run the below query:
 Add more tables in the list if they are enabled in kv

SELECT table_name
FROM information_schema.tables
WHERE table_schema = 'atlas_driver_offer_bpp'
  AND table_type = 'BASE TABLE'
  AND table_name NOT IN (
    'registration_token', 'search_request', 'search_try', 'business_event',
    'booking', 'location', 'location_mapping', 'estimate', 'fare_parameters',
    'fare_parameters_progressive_details', 'ride_details', 'driver_quote',
    'fare_parameters_slab_details', 'rating', 'search_request_for_driver',
    'quote_special_zone', 'payment_order', 'payment_transaction', 'feedback',
    'ride', 'rider_details', 'vehicle_registration_certificate', 'namma_tag'
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
--  Note : -- for Production use or lookup in db to use new tables "useCAC": ["driver_intelligent_pool_config","go_home_config","transporter_config"]
-- Remove "search_request_for_driver" from disableForKV in case of prod, added to make local work.

UPDATE atlas_driver_offer_bpp.system_configs
SET config_value = '{
  "enableKVForRead": ["registration_token","search_request","search_try","business_event","booking","location","location_mapping","estimate","fare_parameters","fare_parameters_progressive_details","ride_details","driver_quote","fare_parameters_slab_details","rating","search_request_for_driver","quote_special_zone","payment_order","payment_transaction","feedback","ride","rider_details","vehicle_registration_certificate","namma_tag"],
  "enableKVForWriteAlso": [{"nameOfTable":"registration_token","percentEnable":100},{"nameOfTable":"search_request","percentEnable":100,"redisTtl":2400},{"nameOfTable":"search_try","percentEnable":100,"redisTtl":2400},{"nameOfTable":"business_event","percentEnable":100,"redisTtl":2400},{"nameOfTable":"booking","percentEnable":100},{"nameOfTable":"location","percentEnable":100},{"nameOfTable":"location_mapping","percentEnable":100},{"nameOfTable":"estimate","percentEnable":100,"redisTtl":2400},{"nameOfTable":"fare_parameters","percentEnable":100},{"nameOfTable":"fare_parameters_progressive_details","percentEnable":100},{"nameOfTable":"ride_details","percentEnable":100},{"nameOfTable":"driver_quote","percentEnable":100,"redisTtl":2400},{"nameOfTable":"fare_parameters_slab_details","percentEnable":100},{"nameOfTable":"rating","percentEnable":100},{"nameOfTable":"search_request_for_driver","percentEnable":100,"redisTtl":2400},{"nameOfTable":"quote_special_zone","percentEnable":100,"redisTtl":2400},{"nameOfTable":"payment_order","percentEnable":100},{"nameOfTable":"payment_transaction","percentEnable":100},{"nameOfTable":"feedback","percentEnable":100},{"nameOfTable":"ride","percentEnable":100},{"nameOfTable":"rider_details","percentEnable":100},{"nameOfTable":"vehicle_registration_certificate","percentEnable":100},{"nameOfTable":"namma_tag","percentEnable":100}],
  "disableForKV": ["issue_config","aadhaar_otp_req","notification","client","subscription_config","mandate","registry_map_fallback","driver_go_home_request","driver_home_location","exophone","driver_pan_card","aadhaar_otp_verify","slab_fare_policy","coin_config","aadhaar_verification","go_home_config","beckn_request","coin_history","black_list_org","booking_cancellation_reason","merchant_state","driver_availability","driver_flow_status","cancellation_reason","driver_intelligent_pool_config","app_dynamic_logic","driver_license","vehicle_service_tier","inter_city_travel_cities","call_status","geometry","lms_module","fare_policy_progressive_details_per_extra_km_rate_section","system_configs","feedback_badge","feedback_form","document_verification_config","image","surge_pricing","issue_report","idfy_verification","issue_option","fare_policy","lms_module_translation","plan_translation","fare_policy_rental_details_distance_buffers","fare_product","lms_module_video_information","lms_video_translation","merchant","value_add_np","search_request_location","merchant_payment_method","message","message_report","message_translation","vehicle_fitness_certificate","invoice","plan","onboarding_document_configs","meta_data","driver_stats","driver_ssn","vehicle_insurance","blocked_route","aadhaar_card","payout_config","leader_board_configs","merchant_push_notification","vehicle","driver_location","driver_rc_association","driver_referral","booking_update_request","beckn_config","restricted_extra_fare","daily_stats","scheduler_job","search_request_special_zone","special_location","purchase_history","special_location_priority","tag_category_mapping","bap_metadata","chakra_queries","toll","merchant_overlay","fare_policy_driver_extra_fee_bounds","payout_order","fare_policy_progressive_details","fare_policy_rental_details","fare_policy_slabs_details_slab","merchant_operating_city","fleet_driver_association","fare_parameters_inter_city_details","person","issue_translation","comment","driver_profile_questions","rider_driver_correlation","coin_purchase_history","gate_info","hyperverge_sdk_logs","payout_transaction","ride_related_notification_config","vehicle_details","white_list_org","merchant_service_usage_config","issue_category","user_data","issue_message","booking_location","driver_fee","merchant_message","fare_policy_inter_city_details_pricing_slabs","app_dynamic_logic_rollout","driver_plan","driver_pool_config","fare_policy_rental_details_pricing_slabs","media_file","time_bound_config","app_dynamic_logic_element","driver_information","transporter_config","search_request_for_driver"],
  "kvTablesTtl": {
    "search_request":2400,
    "search_try":2400,
    "business_event":2400,
    "estimate":2400,
    "driver_quote":2400,
    "search_request_for_driver":2400,
    "quote_special_zone":2400
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

