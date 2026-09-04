-- {"api":"PostFinanceManagementFoTdsReimbursementRequestSubmit","migration":"capability","param":"finance.tds_reimbursement.submit","schema":"atlas_dashboard"}


------- SQL updates -------

-- {"api":"PostFinanceManagementTdsReimbursementRequestSubmit","migration":"capability","param":"finance.tds_reimbursement.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_reimbursement.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/POST_FINANCE_MANAGEMENT_TDS_REIMBURSEMENT_REQUEST_SUBMIT' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFinanceManagementTdsReimbursementStatus","migration":"capability","param":"finance.tds_reimbursement.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_reimbursement.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_TDS_REIMBURSEMENT_STATUS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFinanceManagementTdsReimbursementList","migration":"capability","param":"finance.tds_reimbursement.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_reimbursement.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_TDS_REIMBURSEMENT_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFinanceManagementTdsReimbursement","migration":"capability","param":"finance.tds_reimbursement.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_reimbursement.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_TDS_REIMBURSEMENT' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostFinanceManagementFinanceAdjustmentSubmit","migration":"capability","param":"finance.adjustment.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.adjustment.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/POST_FINANCE_MANAGEMENT_FINANCE_ADJUSTMENT_SUBMIT' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFinanceManagementFinanceAdjustmentList","migration":"capability","param":"finance.adjustment.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.adjustment.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_ADJUSTMENT_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFinanceManagementFinanceAdjustmentApprove","migration":"capability","param":"finance.adjustment.approve","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.adjustment.approve', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/POST_FINANCE_MANAGEMENT_FINANCE_ADJUSTMENT_APPROVE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFinanceManagementFinanceAdjustmentReject","migration":"capability","param":"finance.adjustment.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.adjustment.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/POST_FINANCE_MANAGEMENT_FINANCE_ADJUSTMENT_REJECT' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostFinanceManagementTdsReimbursementReject","migration":"capability","param":"finance.tds_reimbursement.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_reimbursement.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/POST_FINANCE_MANAGEMENT_TDS_REIMBURSEMENT_REJECT' ) ON CONFLICT DO NOTHING;
