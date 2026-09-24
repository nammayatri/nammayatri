-- {"api":"GetInvoiceFinanceList","migration":"capability","param":"finance.report.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.report.read', 'DASHBOARD', 'RIDER_MANAGEMENT/INVOICE/GET_INVOICE_FINANCE_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetInvoiceFinancePdf","migration":"capability","param":"finance.report.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.report.read', 'DASHBOARD', 'RIDER_MANAGEMENT/INVOICE/GET_INVOICE_FINANCE_PDF' ) ON CONFLICT DO NOTHING;
