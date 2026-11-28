INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('system-config.whitelist.write', 'system-config', 'Create operating-city whitelisted network participants', false),
    ('system-config.whitelist.read', 'system-config', 'List operating-city whitelisted network participants', false)
ON CONFLICT (id) DO NOTHING;

INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES
    ('37947162-3b5d-4ed6-bcac-08841be1534d', 'system-config.whitelist.write'),
    ('37947162-3b5d-4ed6-bcac-08841be1534d', 'system-config.whitelist.read')
ON CONFLICT DO NOTHING;

DELETE FROM atlas_dashboard.capability_endpoint
 WHERE capability_id = 'system-config.whitelist.write'
   AND endpoint_id IN (
     'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_OPERATING_CITY_WHITE_LIST_LIST',
     'RIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_OPERATING_CITY_WHITE_LIST_LIST'
   );

DELETE FROM atlas_dashboard.capability_endpoint
 WHERE capability_id = 'city-config.launch.write'
   AND endpoint_id IN (
     'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_WHITE_LIST',
     'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_WHITE_LIST'
   );
