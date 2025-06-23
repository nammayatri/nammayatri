-- {"api":"GetSubscriptionListPlan","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT SUBSCRIPTION LIST_PLAN","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_LIST_PLAN' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'SUBSCRIPTION' AND T1.user_action_type = 'LIST_PLAN' ) ON CONFLICT DO NOTHING;

-- {"api":"PutSubscriptionSelectPlan","migration":"endpoint","param":"SubscriptionAPI SelectPlanEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SELECT_PLAN'
  WHERE endpoint = 'SubscriptionAPI SelectPlanEndpoint';

-- {"api":"PutSubscriptionSelectPlan","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT SUBSCRIPTION SELECT_PLAN","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SELECT_PLAN' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'SUBSCRIPTION' AND T1.user_action_type = 'SELECT_PLAN' ) ON CONFLICT DO NOTHING;

-- {"api":"PutSubscriptionSuspendPlan","migration":"endpoint","param":"SubscriptionAPI SuspendPlanEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SUSPEND_PLAN'
  WHERE endpoint = 'SubscriptionAPI SuspendPlanEndpoint';

-- {"api":"PutSubscriptionSuspendPlan","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT SUBSCRIPTION SUSPEND_PLAN","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SUSPEND_PLAN' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'SUBSCRIPTION' AND T1.user_action_type = 'SUSPEND_PLAN' ) ON CONFLICT DO NOTHING;

-- {"api":"PostSubscriptionSubscribePlan","migration":"endpoint","param":"SubscriptionAPI SubscribePlanEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_SUBSCRIBE_PLAN'
  WHERE endpoint = 'SubscriptionAPI SubscribePlanEndpoint';

-- {"api":"PostSubscriptionSubscribePlan","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT SUBSCRIPTION SUBSCRIBE_PLAN","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_SUBSCRIBE_PLAN' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'SUBSCRIPTION' AND T1.user_action_type = 'SUBSCRIBE_PLAN' ) ON CONFLICT DO NOTHING;

-- {"api":"GetSubscriptionCurrentPlan","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT SUBSCRIPTION CURRENT_PLAN","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_CURRENT_PLAN' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'SUBSCRIPTION' AND T1.user_action_type = 'CURRENT_PLAN' ) ON CONFLICT DO NOTHING;

-- {"api":"GetSubscriptionListPlanV2","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT SUBSCRIPTION LIST_PLAN_V2","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_LIST_PLAN_V2' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'SUBSCRIPTION' AND T1.user_action_type = 'LIST_PLAN_V2' ) ON CONFLICT DO NOTHING;

-- {"api":"PutSubscriptionSelectPlanV2","migration":"endpoint","param":"SubscriptionAPI SelectPlanEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SELECT_PLAN_V2'
  WHERE endpoint = 'SubscriptionAPI SelectPlanEndpoint';

-- {"api":"PutSubscriptionSelectPlanV2","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT SUBSCRIPTION SELECT_PLAN_V2","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SELECT_PLAN_V2' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'SUBSCRIPTION' AND T1.user_action_type = 'SELECT_PLAN_V2' ) ON CONFLICT DO NOTHING;

-- {"api":"PutSubscriptionSuspendPlanV2","migration":"endpoint","param":"SubscriptionAPI SuspendPlanEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SUSPEND_PLAN_V2'
  WHERE endpoint = 'SubscriptionAPI SuspendPlanEndpoint';

-- {"api":"PutSubscriptionSuspendPlanV2","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT SUBSCRIPTION SUSPEND_PLAN_V2","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SUSPEND_PLAN_V2' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'SUBSCRIPTION' AND T1.user_action_type = 'SUSPEND_PLAN_V2' ) ON CONFLICT DO NOTHING;

-- {"api":"PostSubscriptionSubscribePlanV2","migration":"endpoint","param":"SubscriptionAPI SubscribePlanEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_SUBSCRIBE_PLAN_V2'
  WHERE endpoint = 'SubscriptionAPI SubscribePlanEndpoint';

-- {"api":"PostSubscriptionSubscribePlanV2","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT SUBSCRIPTION SUBSCRIBE_PLAN_V2","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_SUBSCRIBE_PLAN_V2' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'SUBSCRIPTION' AND T1.user_action_type = 'SUBSCRIBE_PLAN_V2' ) ON CONFLICT DO NOTHING;

-- {"api":"GetSubscriptionCurrentPlanV2","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT SUBSCRIPTION CURRENT_PLAN_V2","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_CURRENT_PLAN_V2' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'SUBSCRIPTION' AND T1.user_action_type = 'CURRENT_PLAN_V2' ) ON CONFLICT DO NOTHING;

-- {"api":"GetSubscriptionOrderStatus","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT SUBSCRIPTION PAYMENT_STATUS","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_ORDER_STATUS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'SUBSCRIPTION' AND T1.user_action_type = 'PAYMENT_STATUS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetSubscriptionDriverPaymentHistoryAPIV2","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT SUBSCRIPTION PAYMENT_HISTORY_V2","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_DRIVER_PAYMENT_HISTORY_API_V2' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'SUBSCRIPTION' AND T1.user_action_type = 'PAYMENT_HISTORY_V2' ) ON CONFLICT DO NOTHING;

-- {"api":"GetSubscriptionDriverPaymentHistoryEntityDetailsV2","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT SUBSCRIPTION PAYMENT_HISTORY_ENTITY_DETAILS_V2","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_DRIVER_PAYMENT_HISTORY_ENTITY_DETAILS_V2' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'SUBSCRIPTION' AND T1.user_action_type = 'PAYMENT_HISTORY_ENTITY_DETAILS_V2' ) ON CONFLICT DO NOTHING;

-- {"api":"PostSubscriptionCollectManualPayments","migration":"endpoint","param":"SubscriptionAPI CollectPaymentsEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_COLLECT_MANUAL_PAYMENTS'
  WHERE endpoint = 'SubscriptionAPI CollectPaymentsEndPoint';

-- {"api":"PostSubscriptionCollectManualPayments","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT SUBSCRIPTION COLLECT_MANUAL_PAYMENTS","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_COLLECT_MANUAL_PAYMENTS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'SUBSCRIPTION' AND T1.user_action_type = 'COLLECT_MANUAL_PAYMENTS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostSubscriptionFeeWaiveOff","migration":"endpoint","param":"SubscriptionAPI FeeWaiveOffEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_FEE_WAIVE_OFF'
  WHERE endpoint = 'SubscriptionAPI FeeWaiveOffEndPoint';

-- {"api":"PostSubscriptionFeeWaiveOff","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT SUBSCRIPTION FEE_WAIVE_OFF","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_FEE_WAIVE_OFF' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'SUBSCRIPTION' AND T1.user_action_type = 'FEE_WAIVE_OFF' ) ON CONFLICT DO NOTHING;
