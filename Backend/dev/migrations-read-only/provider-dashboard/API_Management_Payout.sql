-- {"api":"PostPayoutPayoutVerifyFraudStatus","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_VERIFY_FRAUD_STATUS'
  WHERE endpoint = 'PayoutAPI PostPayoutPayoutVerifyFraudStatusEndpoint';

-- {"api":"PostPayoutPayoutRetryFailed","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_RETRY_FAILED'
  WHERE endpoint = 'PayoutAPI PostPayoutPayoutRetryFailedEndpoint';

-- {"api":"PostPayoutPayoutRetryAllWithStatus","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_RETRY_ALL_WITH_STATUS'
  WHERE endpoint = 'PayoutAPI PostPayoutPayoutRetryAllWithStatusEndpoint';


------- SQL updates -------

-- {"api":"PostPayoutPayoutPendingPayout","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_PENDING_PAYOUT'
  WHERE endpoint = 'PayoutAPI PostPayoutPayoutPendingPayoutEndpoint';


------- SQL updates -------

-- {"api":"PostPayoutPayoutDeleteVPA","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_DELETE_VPA'
  WHERE endpoint = 'PayoutAPI PostPayoutPayoutDeleteVPAEndpoint';
