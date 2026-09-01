-- {"api":"GetPayoutPayoutAdhocEligibility","migration":"capability","param":"finance.payout.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.payout.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_PAYOUT_ADHOC_ELIGIBILITY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPayoutPayoutAdhocInitiate","migration":"capability","param":"finance.payout.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.payout.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_ADHOC_INITIATE' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetPayoutPayoutBatchList","migration":"capability","param":"finance.payout.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.payout.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_PAYOUT_BATCH_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetPayoutPayoutBatchOrders","migration":"capability","param":"finance.payout.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.payout.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_PAYOUT_BATCH_ORDERS' ) ON CONFLICT DO NOTHING;
