-- {"api":"GetPayoutPayoutScheduledPayoutConfig","migration":"capability","param":"finance.payout.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.payout.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_PAYOUT_SCHEDULED_PAYOUT_CONFIG' ) ON CONFLICT DO NOTHING;

-- {"api":"GetPayoutAdhocLookup","migration":"capability","param":"finance.payout.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.payout.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_ADHOC_LOOKUP' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPayoutAdhocInitiate","migration":"capability","param":"finance.payout.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.payout.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_ADHOC_INITIATE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetPayoutBatchList","migration":"capability","param":"finance.payout.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.payout.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_BATCH_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetPayoutBatchOrders","migration":"capability","param":"finance.payout.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.payout.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_BATCH_ORDERS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetPayoutExcluded","migration":"capability","param":"finance.payout.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.payout.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_EXCLUDED' ) ON CONFLICT DO NOTHING;
