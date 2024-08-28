-- {"api":"PostPayoutPayoutVerifyFraudStatus","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_PayoutAPI_PostPayoutPayoutVerifyFraudStatusEndpoint'
  WHERE endpoint = 'PayoutAPI PostPayoutPayoutVerifyFraudStatusEndpoint';

-- {"api":"PostPayoutPayoutRetryFailed","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_PayoutAPI_PostPayoutPayoutRetryFailedEndpoint'
  WHERE endpoint = 'PayoutAPI PostPayoutPayoutRetryFailedEndpoint';

-- {"api":"PostPayoutPayoutRetryAllWithStatus","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_PayoutAPI_PostPayoutPayoutRetryAllWithStatusEndpoint'
  WHERE endpoint = 'PayoutAPI PostPayoutPayoutRetryAllWithStatusEndpoint';
