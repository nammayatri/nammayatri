-- {"api":"PostDriverSubscriptionSendSms","migration":"endpoint","param":"DriverAPI SendMessageToDriverViaDashboardEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/DRIVER_SUBSCRIPTION/POST_DRIVER_SUBSCRIPTION_SEND_SMS'
  WHERE endpoint = 'DriverAPI SendMessageToDriverViaDashboardEndPoint';

-- {"api":"PostDriverSubscriptionSendSms","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS SEND_DASHBOARD_MESSAGE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/DRIVER_SUBSCRIPTION/POST_DRIVER_SUBSCRIPTION_SEND_SMS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'SEND_DASHBOARD_MESSAGE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverSubscriptionUpdateDriverFeeAndInvoiceInfo","migration":"endpoint","param":"DriverAPI UpdateSubscriptionDriverFeeAndInvoiceEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/DRIVER_SUBSCRIPTION/POST_DRIVER_SUBSCRIPTION_UPDATE_DRIVER_FEE_AND_INVOICE_INFO'
  WHERE endpoint = 'DriverAPI UpdateSubscriptionDriverFeeAndInvoiceEndpoint';

-- {"api":"PostDriverSubscriptionUpdateDriverFeeAndInvoiceInfo","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS DRIVER_SUBSCRIPTION_DRIVER_FEE_AND_INVOICE_UPDATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/DRIVER_SUBSCRIPTION/POST_DRIVER_SUBSCRIPTION_UPDATE_DRIVER_FEE_AND_INVOICE_INFO' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'DRIVER_SUBSCRIPTION_DRIVER_FEE_AND_INVOICE_UPDATE' ) ON CONFLICT DO NOTHING;
