-- {"api":"PostDriverSubscriptionSendSms","migration":"endpoint","param":"DriverAPI SendMessageToDriverViaDashboardEndPoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/DRIVER_SUBSCRIPTION/POST_DRIVER_SUBSCRIPTION_SEND_SMS'
  WHERE endpoint = 'DriverAPI SendMessageToDriverViaDashboardEndPoint';

-- {"api":"PostDriverSubscriptionUpdateDriverFeeAndInvoiceInfo","migration":"endpoint","param":"DriverAPI UpdateSubscriptionDriverFeeAndInvoiceEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/DRIVER_SUBSCRIPTION/POST_DRIVER_SUBSCRIPTION_UPDATE_DRIVER_FEE_AND_INVOICE_INFO'
  WHERE endpoint = 'DriverAPI UpdateSubscriptionDriverFeeAndInvoiceEndpoint';
