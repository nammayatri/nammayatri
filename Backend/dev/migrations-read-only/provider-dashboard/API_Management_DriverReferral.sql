-- {"api":"PostDriverReferralReferralOpsPassword","migration":"endpoint","param":"DriverReferralAPI ReferralProgramUpdateOpsPasswordEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REFERRAL/POST_DRIVER_REFERRAL_REFERRAL_OPS_PASSWORD'
  WHERE endpoint = 'DriverReferralAPI ReferralProgramUpdateOpsPasswordEndpoint';

-- {"api":"PostDriverReferralReferralOpsPassword","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REFERRAL/POST_DRIVER_REFERRAL_REFERRAL_OPS_PASSWORD'
  WHERE endpoint = 'DriverReferralAPI PostDriverReferralReferralOpsPasswordEndpoint';

-- {"api":"PostDriverReferralLinkReferral","migration":"endpoint","param":"DriverReferralAPI ReferralProgramUpdateOpsPasswordEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REFERRAL/POST_DRIVER_REFERRAL_LINK_REFERRAL'
  WHERE endpoint = 'DriverReferralAPI ReferralProgramUpdateOpsPasswordEndpoint';

-- {"api":"PostDriverReferralLinkReferral","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REFERRAL/POST_DRIVER_REFERRAL_LINK_REFERRAL'
  WHERE endpoint = 'DriverReferralAPI PostDriverReferralLinkReferralEndpoint';


------- SQL updates -------

-- {"api":"PostDriverReferralReferralOpsPassword","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT REFERRAL REFERRAL_PROGRAM_PASSWORD_UPDATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_REFERRAL/POST_DRIVER_REFERRAL_REFERRAL_OPS_PASSWORD' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'REFERRAL' AND T1.user_action_type = 'REFERRAL_PROGRAM_PASSWORD_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverReferralLinkReferral","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT REFERRAL REFERRAL_PROGRAM_LINK_CODE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_REFERRAL/POST_DRIVER_REFERRAL_LINK_REFERRAL' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'REFERRAL' AND T1.user_action_type = 'REFERRAL_PROGRAM_LINK_CODE' ) ON CONFLICT DO NOTHING;
