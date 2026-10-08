-- {"api":"GetRSFReconciliationRsfOrders","migration":"capability","param":"financeManagement.rsfOrders.list","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfOrders.list', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_ORDERS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfUtrs","migration":"capability","param":"financeManagement.rsfUtrs.list","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfUtrs.list', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_UTRS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfUtr","migration":"capability","param":"financeManagement.rsfUtrs.detail","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfUtrs.detail', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_UTR' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRSFReconciliationRsfUtrBankVerify","migration":"capability","param":"financeManagement.rsfUtrs.bankVerify","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfUtrs.bankVerify', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/POST_RSF_RECONCILIATION_RSF_UTR_BANK_VERIFY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRSFReconciliationRsfAutoAllocation","migration":"capability","param":"financeManagement.rsfAllocation.auto","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfAllocation.auto', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/POST_RSF_RECONCILIATION_RSF_AUTO_ALLOCATION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRSFReconciliationRsfOrdersConfirm","migration":"capability","param":"financeManagement.rsfOrders.confirm","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfOrders.confirm', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/POST_RSF_RECONCILIATION_RSF_ORDERS_CONFIRM' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRSFReconciliationRsfSend","migration":"capability","param":"financeManagement.rsfSettlements.send","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfSettlements.send', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/POST_RSF_RECONCILIATION_RSF_SEND' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfReconUnmatched","migration":"capability","param":"financeManagement.rsfRecon.unmatched","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfRecon.unmatched', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_RECON_UNMATCHED' ) ON CONFLICT DO NOTHING;
