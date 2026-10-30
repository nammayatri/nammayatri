-- {"api":"GetRSFReconciliationRsfOrders","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfOrders.list' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfSummary","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfSummary.read' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfUtrs","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfUtrs.list' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfUtr","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfUtrs.detail' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRSFReconciliationRsfUtrBankVerify","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfUtrs.bankVerify' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRSFReconciliationRsfAutoAllocation","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfAllocation.auto' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRSFReconciliationRsfOrdersConfirm","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfOrders.confirm' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRSFReconciliationRsfAllocationsManual","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfOrders.confirm' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRSFReconciliationRsfSend","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfSettlements.send' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfReconUnmatched","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfRecon.unmatched' ) ON CONFLICT DO NOTHING;
