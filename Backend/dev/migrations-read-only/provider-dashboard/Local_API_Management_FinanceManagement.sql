
------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

-- {"api":"PostFinanceManagementTdsReimbursementRequestSubmit","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'finance.tds_reimbursement.write' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFinanceManagementTdsReimbursementStatus","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'finance.tds_reimbursement.read' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFinanceManagementTdsReimbursementList","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'finance.tds_reimbursement.read' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFinanceManagementTdsReimbursement","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'finance.tds_reimbursement.read' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostFinanceManagementFinanceAdjustmentSubmit","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'finance.adjustment.write' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFinanceManagementFinanceAdjustmentList","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'finance.adjustment.read' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFinanceManagementFinanceAdjustmentApprove","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'finance.adjustment.approve' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFinanceManagementFinanceAdjustmentReject","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'finance.adjustment.write' ) ON CONFLICT DO NOTHING;

------- SQL updates -------

-- {"api":"PostFinanceManagementTdsReimbursementReject","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'finance.tds_reimbursement.write' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetFinanceManagementSubscriptionPurchaseList","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFinanceManagementFinanceInvoicePdf","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFinanceManagementFinanceInvoiceList","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFinanceManagementFinanceAuditList","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFinanceManagementFinanceReconciliation","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFinanceManagementFinancePaymentSettlementList","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFinanceManagementFinancePaymentGatewayTransactionList","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFinanceManagementFinanceWalletLedger","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostFinanceManagementReconciliationTrigger","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFinanceManagementFinanceSapJournals","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFinanceManagementFinanceSapJournalsTransactions","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.
