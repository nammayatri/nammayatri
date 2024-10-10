-- {"api":"DeleteCustomerDelete","migration":"endpoint","param":"CustomerAPI DeleteCustomerEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/DELETE_CUSTOMER_DELETE'
  WHERE endpoint = 'CustomerAPI DeleteCustomerEndpoint';


------- SQL updates -------

-- {"api":"PostCustomerBlock","migration":"endpoint","param":"CustomerAPI BlockCustomerEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_BLOCK'
  WHERE endpoint = 'CustomerAPI BlockCustomerEndpoint';


------- SQL updates -------

-- {"api":"PostCustomerUnblock","migration":"endpoint","param":"CustomerAPI UnblockCustomerEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_UNBLOCK'
  WHERE endpoint = 'CustomerAPI UnblockCustomerEndpoint';


------- SQL updates -------

-- {"api":"PostCustomerCancellationDuesSync","migration":"endpoint","param":"CustomerAPI CancellationDuesSyncCustomerEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_CANCELLATION_DUES_SYNC'
  WHERE endpoint = 'CustomerAPI CancellationDuesSyncCustomerEndpoint';


------- SQL updates -------

-- {"api":"PostCustomerUpdateSafetyCenterBlocking","migration":"endpoint","param":"CustomerAPI UpdateSafetyCenterBlockingCustomerEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_UPDATE_SAFETY_CENTER_BLOCKING'
  WHERE endpoint = 'CustomerAPI UpdateSafetyCenterBlockingCustomerEndpoint';


------- SQL updates -------

-- {"api":"PostCustomerPersonNumbers","migration":"endpoint","param":"CustomerAPI PersonNumbersCustomerEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_PERSON_NUMBERS'
  WHERE endpoint = 'CustomerAPI PersonNumbersCustomerEndpoint';


------- SQL updates -------

-- {"api":"PostCustomerPersonId","migration":"endpoint","param":"CustomerAPI PersonIdCustomerEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_PERSON_ID'
  WHERE endpoint = 'CustomerAPI PersonIdCustomerEndpoint';
