-- {"api":"DeleteCustomerDelete","migration":"endpoint","param":"CustomerAPI DeleteCustomerEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/DELETE_CUSTOMER_DELETE'
  WHERE endpoint = 'CustomerAPI DeleteCustomerEndpoint';


------- SQL updates -------

-- {"api":"PostCustomerBlock","migration":"endpoint","param":"CustomerAPI BlockCustomerEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_BLOCK'
  WHERE endpoint = 'CustomerAPI BlockCustomerEndpoint';
