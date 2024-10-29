-- {"api":"GetCustomerList","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS CUSTOMER_LIST","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/CUSTOMER/GET_CUSTOMER_LIST' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'CUSTOMER_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"DeleteCustomerDelete","migration":"endpoint","param":"CustomerAPI DeleteCustomerEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/DELETE_CUSTOMER_DELETE'
  WHERE endpoint = 'CustomerAPI DeleteCustomerEndpoint';

-- {"api":"DeleteCustomerDelete","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS CUSTOMER_DELETE","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/CUSTOMER/DELETE_CUSTOMER_DELETE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'CUSTOMER_DELETE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostCustomerBlock","migration":"endpoint","param":"CustomerAPI BlockCustomerEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_BLOCK'
  WHERE endpoint = 'CustomerAPI BlockCustomerEndpoint';

-- {"api":"PostCustomerBlock","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS CUSTOMER_BLOCK","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_BLOCK' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'CUSTOMER_BLOCK' ) ON CONFLICT DO NOTHING;

-- {"api":"PostCustomerUnblock","migration":"endpoint","param":"CustomerAPI UnblockCustomerEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_UNBLOCK'
  WHERE endpoint = 'CustomerAPI UnblockCustomerEndpoint';

-- {"api":"PostCustomerUnblock","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS CUSTOMER_UNBLOCK","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_UNBLOCK' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'CUSTOMER_UNBLOCK' ) ON CONFLICT DO NOTHING;

-- {"api":"GetCustomerInfo","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS CUSTOMER_INFO","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/CUSTOMER/GET_CUSTOMER_INFO' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'CUSTOMER_INFO' ) ON CONFLICT DO NOTHING;

-- {"api":"PostCustomerCancellationDuesSync","migration":"endpoint","param":"CustomerAPI CancellationDuesSyncCustomerEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_CANCELLATION_DUES_SYNC'
  WHERE endpoint = 'CustomerAPI CancellationDuesSyncCustomerEndpoint';

-- {"api":"PostCustomerCancellationDuesSync","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS CUSTOMER_CANCELLATION_DUES_SYNC","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_CANCELLATION_DUES_SYNC' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'CUSTOMER_CANCELLATION_DUES_SYNC' ) ON CONFLICT DO NOTHING;

-- {"api":"GetCustomerCancellationDuesDetails","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS CUSTOMER_CANCELLATION_DUES_DETAILS","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/CUSTOMER/GET_CUSTOMER_CANCELLATION_DUES_DETAILS' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'CUSTOMER_CANCELLATION_DUES_DETAILS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostCustomerUpdateSafetyCenterBlocking","migration":"endpoint","param":"CustomerAPI UpdateSafetyCenterBlockingCustomerEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_UPDATE_SAFETY_CENTER_BLOCKING'
  WHERE endpoint = 'CustomerAPI UpdateSafetyCenterBlockingCustomerEndpoint';

-- {"api":"PostCustomerUpdateSafetyCenterBlocking","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS UPDATE_SAFETY_CENTER","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_UPDATE_SAFETY_CENTER_BLOCKING' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'UPDATE_SAFETY_CENTER' ) ON CONFLICT DO NOTHING;

-- {"api":"PostCustomerPersonNumbers","migration":"endpoint","param":"CustomerAPI PersonNumbersCustomerEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_PERSON_NUMBERS'
  WHERE endpoint = 'CustomerAPI PersonNumbersCustomerEndpoint';

-- {"api":"PostCustomerPersonNumbers","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS PERSON_NUMBERS","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_PERSON_NUMBERS' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'PERSON_NUMBERS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostCustomerPersonId","migration":"endpoint","param":"CustomerAPI PersonIdCustomerEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_PERSON_ID'
  WHERE endpoint = 'CustomerAPI PersonIdCustomerEndpoint';

-- {"api":"PostCustomerPersonId","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS FETCH_PERSON_ID","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_PERSON_ID' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'FETCH_PERSON_ID' ) ON CONFLICT DO NOTHING;
