-- {"api":"PostCustomerSosCreate","migration":"endpoint","param":"SosAPI CreateSosEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_APP_MANAGEMENT/CUSTOMER/POST_CUSTOMER_SOS_CREATE'
  WHERE endpoint = 'SosAPI CreateSosEndpoint';

-- {"api":"PostCustomerSosCreate","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS CREATE_SOS","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/CUSTOMER/POST_CUSTOMER_SOS_CREATE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'CREATE_SOS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostCustomerDeletedPerson","migration":"endpoint","param":"DeletedPersonAPI CreateDeletedPersonEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_APP_MANAGEMENT/CUSTOMER/POST_CUSTOMER_DELETED_PERSON'
  WHERE endpoint = 'DeletedPersonAPI CreateDeletedPersonEndpoint';

-- {"api":"PostCustomerDeletedPerson","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS CREATE_DELETED_PERSON","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/CUSTOMER/POST_CUSTOMER_DELETED_PERSON' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'CREATE_DELETED_PERSON' ) ON CONFLICT DO NOTHING;
