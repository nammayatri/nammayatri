INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CLOUD_UPDATE')
ON CONFLICT DO NOTHING;
