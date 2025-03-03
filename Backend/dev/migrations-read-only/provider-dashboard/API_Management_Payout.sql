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


------- SQL updates -------

-- {"api":"GetPayoutPayoutReferralHistory","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS PAYOUT_MANAGEMENT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_PAYOUT_REFERRAL_HISTORY' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'PAYOUT_MANAGEMENT' ) ON CONFLICT DO NOTHING;

-- {"api":"GetPayoutPayoutHistory","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS PAYOUT_MANAGEMENT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_PAYOUT_HISTORY' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'PAYOUT_MANAGEMENT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPayoutPayoutVerifyFraudStatus","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS PAYOUT_MANAGEMENT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_VERIFY_FRAUD_STATUS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'PAYOUT_MANAGEMENT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPayoutPayoutRetryFailed","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS PAYOUT_MANAGEMENT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_RETRY_FAILED' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'PAYOUT_MANAGEMENT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPayoutPayoutRetryAllWithStatus","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS PAYOUT_MANAGEMENT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_RETRY_ALL_WITH_STATUS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'PAYOUT_MANAGEMENT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPayoutPayoutPendingPayout","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS PAYOUT_MANAGEMENT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_PENDING_PAYOUT' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'PAYOUT_MANAGEMENT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPayoutPayoutDeleteVPA","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS PAYOUT_MANAGEMENT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_DELETE_VPA' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'PAYOUT_MANAGEMENT' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostPayoutPayoutDriversSetBlockState","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS PAYOUT_MANAGEMENT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_DRIVERS_SET_BLOCK_STATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'PAYOUT_MANAGEMENT' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostPayoutPayoutUpdateVPA","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS PAYOUT_MANAGEMENT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_UPDATE_VPA' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'PAYOUT_MANAGEMENT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPayoutPayoutRefundRegistrationAmount","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS PAYOUT_MANAGEMENT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_REFUND_REGISTRATION_AMOUNT' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'PAYOUT_MANAGEMENT' ) ON CONFLICT DO NOTHING;
