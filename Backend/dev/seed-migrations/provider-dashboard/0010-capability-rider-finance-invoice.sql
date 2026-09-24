INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES
    ('finance.report.read', 'DASHBOARD', 'RIDER_MANAGEMENT/INVOICE/GET_INVOICE_FINANCE_LIST'),
    ('finance.report.read', 'DASHBOARD', 'RIDER_MANAGEMENT/INVOICE/GET_INVOICE_FINANCE_PDF')
ON CONFLICT DO NOTHING;
