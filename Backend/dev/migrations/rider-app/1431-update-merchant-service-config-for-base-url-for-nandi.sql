UPDATE atlas_app.merchant_service_config
SET config_json = jsonb_set(
    config_json::jsonb,
    '{baseUrl}',
    '"http://beckn-nandi-master.atlas.svc.cluster.local:8080"'::jsonb
)::json
WHERE service_name = 'MultiModal_OTPTransit';