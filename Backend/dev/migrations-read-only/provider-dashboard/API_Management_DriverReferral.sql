-- {"api":"PostDriverReferralReferralOpsPassword","migration":"endpoint","param":"DriverReferralAPI ReferralProgramUpdateOpsPasswordEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverReferralAPI_PostDriverReferralReferralOpsPasswordEndpoint'
  WHERE endpoint = 'DriverReferralAPI ReferralProgramUpdateOpsPasswordEndpoint';

-- {"api":"PostDriverReferralReferralOpsPassword","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverReferralAPI_PostDriverReferralReferralOpsPasswordEndpoint'
  WHERE endpoint = 'DriverReferralAPI PostDriverReferralReferralOpsPasswordEndpoint';

-- {"api":"PostDriverReferralLinkReferral","migration":"endpoint","param":"DriverReferralAPI ReferralProgramUpdateOpsPasswordEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverReferralAPI_PostDriverReferralLinkReferralEndpoint'
  WHERE endpoint = 'DriverReferralAPI ReferralProgramUpdateOpsPasswordEndpoint';

-- {"api":"PostDriverReferralLinkReferral","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverReferralAPI_PostDriverReferralLinkReferralEndpoint'
  WHERE endpoint = 'DriverReferralAPI PostDriverReferralLinkReferralEndpoint';
