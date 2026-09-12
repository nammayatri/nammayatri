-- {"api":"PostCustomerSosCreate","migration":"endpoint","param":"SosAPI CreateSosEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_APP_MANAGEMENT/CUSTOMER/POST_CUSTOMER_SOS_CREATE'
  WHERE endpoint = 'SosAPI CreateSosEndpoint';

------- SQL updates -------

-- {"api":"PostCustomerDeletedPerson","migration":"endpoint","param":"DeletedPersonAPI CreateDeletedPersonEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_APP_MANAGEMENT/CUSTOMER/POST_CUSTOMER_DELETED_PERSON'
  WHERE endpoint = 'DeletedPersonAPI CreateDeletedPersonEndpoint';
