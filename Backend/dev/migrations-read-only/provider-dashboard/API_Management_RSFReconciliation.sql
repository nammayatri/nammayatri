-- {"api":"GetRSFReconciliationRsfSettlements","migration":"capability","param":"PUBLIC","schema":"atlas_dashboard"}
-- capability: PUBLIC - no capability_endpoint row; every authenticated caller may call this endpoint.

-- {"api":"GetRSFReconciliationRsfSettlementsUtrs","migration":"capability","param":"PUBLIC","schema":"atlas_dashboard"}
-- capability: PUBLIC - no capability_endpoint row; every authenticated caller may call this endpoint.

-- {"api":"GetRSFReconciliationRsfSettlementsOrders","migration":"capability","param":"PUBLIC","schema":"atlas_dashboard"}
-- capability: PUBLIC - no capability_endpoint row; every authenticated caller may call this endpoint.

-- {"api":"PostRSFReconciliationRsfSettlementsSend","migration":"capability","param":"PUBLIC","schema":"atlas_dashboard"}
-- capability: PUBLIC - no capability_endpoint row; every authenticated caller may call this endpoint.

-- {"api":"GetRSFReconciliationRsfUtrs","migration":"capability","param":"PUBLIC","schema":"atlas_dashboard"}
-- capability: PUBLIC - no capability_endpoint row; every authenticated caller may call this endpoint.

-- {"api":"GetRSFReconciliationRsfUtrs","migration":"capability","param":"PUBLIC","schema":"atlas_dashboard"}
-- capability: PUBLIC - no capability_endpoint row; every authenticated caller may call this endpoint.

-- {"api":"PostRSFReconciliationRsfUtrsBank-verify","migration":"capability","param":"PUBLIC","schema":"atlas_dashboard"}
-- capability: PUBLIC - no capability_endpoint row; every authenticated caller may call this endpoint.

-- {"api":"PostRSFReconciliationRsfOrdersConfirm","migration":"capability","param":"PUBLIC","schema":"atlas_dashboard"}
-- capability: PUBLIC - no capability_endpoint row; every authenticated caller may call this endpoint.


------- SQL updates -------

-- {"api":"PostRSFReconciliationRsfUtrsBankVerify","migration":"capability","param":"PUBLIC","schema":"atlas_dashboard"}
-- capability: PUBLIC - no capability_endpoint row; every authenticated caller may call this endpoint.


------- SQL updates -------

-- {"api":"GetRSFReconciliationRsfUtr","migration":"capability","param":"PUBLIC","schema":"atlas_dashboard"}
-- capability: PUBLIC - no capability_endpoint row; every authenticated caller may call this endpoint.

-- {"api":"PostRSFReconciliationRsfUtrBankVerify","migration":"capability","param":"PUBLIC","schema":"atlas_dashboard"}
-- capability: PUBLIC - no capability_endpoint row; every authenticated caller may call this endpoint.


------- SQL updates -------

-- {"api":"PostRSFReconciliationRsfUtrsVerify","migration":"capability","param":"PUBLIC","schema":"atlas_dashboard"}
-- capability: PUBLIC - no capability_endpoint row; every authenticated caller may call this endpoint.


------- SQL updates -------

-- {"api":"GetRSFReconciliationRsfReconGrid","migration":"capability","param":"PUBLIC","schema":"atlas_dashboard"}
-- capability: PUBLIC - no capability_endpoint row; every authenticated caller may call this endpoint.

-- {"api":"GetRSFReconciliationRsfReconUnmatched","migration":"capability","param":"PUBLIC","schema":"atlas_dashboard"}
-- capability: PUBLIC - no capability_endpoint row; every authenticated caller may call this endpoint.


------- SQL updates -------

-- {"api":"GetRSFReconciliationRsfSettlements","migration":"capability","param":"financeManagement.rsfSettlements.list","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfSettlements.list', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_SETTLEMENTS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfSettlementsUtrs","migration":"capability","param":"financeManagement.rsfSettlements.utrs","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfSettlements.utrs', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_SETTLEMENTS_UTRS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfSettlementsOrders","migration":"capability","param":"financeManagement.rsfSettlements.orders","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfSettlements.orders', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_SETTLEMENTS_ORDERS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRSFReconciliationRsfSettlementsSend","migration":"capability","param":"financeManagement.rsfSettlements.send","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'financeManagement.rsfSettlements.send', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RSF_RECONCILIATION/POST_RSF_RECONCILIATION_RSF_SETTLEMENTS_SEND' ) ON CONFLICT DO NOTHING;

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
