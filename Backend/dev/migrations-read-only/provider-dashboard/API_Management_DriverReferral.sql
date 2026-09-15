-- {"api":"PostDriverReferralReferralOpsPassword","migration":"endpoint","param":"DriverReferralAPI ReferralProgramUpdateOpsPasswordEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REFERRAL/POST_DRIVER_REFERRAL_REFERRAL_OPS_PASSWORD'
  WHERE endpoint = 'DriverReferralAPI ReferralProgramUpdateOpsPasswordEndpoint';

-- {"api":"PostDriverReferralReferralOpsPassword","migration":"endpointV2","param":null,"schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REFERRAL/POST_DRIVER_REFERRAL_REFERRAL_OPS_PASSWORD'
  WHERE endpoint = 'DriverReferralAPI PostDriverReferralReferralOpsPasswordEndpoint';

-- {"api":"PostDriverReferralLinkReferral","migration":"endpoint","param":"DriverReferralAPI ReferralProgramUpdateOpsPasswordEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REFERRAL/POST_DRIVER_REFERRAL_LINK_REFERRAL'
  WHERE endpoint = 'DriverReferralAPI ReferralProgramUpdateOpsPasswordEndpoint';

-- {"api":"PostDriverReferralLinkReferral","migration":"endpointV2","param":null,"schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REFERRAL/POST_DRIVER_REFERRAL_LINK_REFERRAL'
  WHERE endpoint = 'DriverReferralAPI PostDriverReferralLinkReferralEndpoint';

------- SQL updates -------
