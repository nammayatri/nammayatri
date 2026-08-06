-- {"api":"GetRSFReconciliationRsfMessages","migration":"capability","param":"financeManagement.rsfSettlements.list","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfSettlements.list', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_MESSAGES' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfMessagesUtrs","migration":"capability","param":"financeManagement.rsfSettlements.utrs","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfSettlements.utrs', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_MESSAGES_UTRS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfMessagesOrders","migration":"capability","param":"financeManagement.rsfSettlements.orders","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfSettlements.orders', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_MESSAGES_ORDERS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRSFReconciliationRsfMessagesSend","migration":"capability","param":"financeManagement.rsfSettlements.send","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfSettlements.send', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/POST_RSF_RECONCILIATION_RSF_MESSAGES_SEND' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfUtrs","migration":"capability","param":"financeManagement.rsfUtrs.list","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfUtrs.list', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_UTRS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfUtr","migration":"capability","param":"financeManagement.rsfUtrs.detail","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfUtrs.detail', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_UTR' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRSFReconciliationRsfUtrBankVerify","migration":"capability","param":"financeManagement.rsfUtrs.bankVerify","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfUtrs.bankVerify', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/POST_RSF_RECONCILIATION_RSF_UTR_BANK_VERIFY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRSFReconciliationRsfOrdersConfirm","migration":"capability","param":"financeManagement.rsfOrders.confirm","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfOrders.confirm', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/POST_RSF_RECONCILIATION_RSF_ORDERS_CONFIRM' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfReconGrid","migration":"capability","param":"financeManagement.rsfRecon.grid","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfRecon.grid', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_RECON_GRID' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfReconUnmatched","migration":"capability","param":"financeManagement.rsfRecon.unmatched","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfRecon.unmatched', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_RECON_UNMATCHED' ) ON CONFLICT DO NOTHING;
