-- {"api":"DeleteCustomerDelete","migration":"endpoint","param":"CustomerAPI DeleteCustomerEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/DELETE_CUSTOMER_DELETE'
  WHERE endpoint = 'CustomerAPI DeleteCustomerEndpoint';

-- {"api":"DeleteCustomerDelete","migration":"endpointV2","param":null,"schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/DELETE_CUSTOMER_DELETE'
  WHERE endpoint = 'CustomerAPI DeleteCustomerDeleteEndpoint';
