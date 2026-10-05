
-- {"api":"PutSubscriptionSelectPlan","migration":"endpoint","param":"SubscriptionAPI SelectPlanEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SELECT_PLAN'
  WHERE endpoint = 'SubscriptionAPI SelectPlanEndpoint';

-- {"api":"PutSubscriptionSuspendPlan","migration":"endpoint","param":"SubscriptionAPI SuspendPlanEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SUSPEND_PLAN'
  WHERE endpoint = 'SubscriptionAPI SuspendPlanEndpoint';

-- {"api":"PostSubscriptionSubscribePlan","migration":"endpoint","param":"SubscriptionAPI SubscribePlanEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_SUBSCRIBE_PLAN'
  WHERE endpoint = 'SubscriptionAPI SubscribePlanEndpoint';

-- {"api":"PutSubscriptionSelectPlanV2","migration":"endpoint","param":"SubscriptionAPI SelectPlanEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SELECT_PLAN_V2'
  WHERE endpoint = 'SubscriptionAPI SelectPlanEndpoint';

-- {"api":"PutSubscriptionSuspendPlanV2","migration":"endpoint","param":"SubscriptionAPI SuspendPlanEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SUSPEND_PLAN_V2'
  WHERE endpoint = 'SubscriptionAPI SuspendPlanEndpoint';

-- {"api":"PostSubscriptionSubscribePlanV2","migration":"endpoint","param":"SubscriptionAPI SubscribePlanEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_SUBSCRIBE_PLAN_V2'
  WHERE endpoint = 'SubscriptionAPI SubscribePlanEndpoint';

-- {"api":"PostSubscriptionCollectManualPayments","migration":"endpoint","param":"SubscriptionAPI CollectPaymentsEndPoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_COLLECT_MANUAL_PAYMENTS'
  WHERE endpoint = 'SubscriptionAPI CollectPaymentsEndPoint';

------- SQL updates -------

-- {"api":"PostSubscriptionFeeWaiveOff","migration":"endpoint","param":"SubscriptionAPI FeeWaiveOffEndPoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_FEE_WAIVE_OFF'
  WHERE endpoint = 'SubscriptionAPI FeeWaiveOffEndPoint';

------- SQL updates -------

-- {"api":"GetSubscriptionCancellationChargeHistory","migration":"capability","param":"city-operations.subscription.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.subscription.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_CANCELLATION_CHARGE_HISTORY' ) ON CONFLICT DO NOTHING;
