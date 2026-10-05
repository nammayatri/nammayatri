INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('city-operations.driver_block.read', 'city-operations', 'View driver block reasons and airport preference', false),
    ('city-operations.driver_block.write', 'city-operations', 'Block / unblock drivers and set airport preference', false)
ON CONFLICT (id) DO NOTHING;

DELETE FROM atlas_dashboard.capability_endpoint
 WHERE capability_id = 'city-operations.driver.read'
   AND server_name = 'DASHBOARD'
   AND endpoint_id IN (
       'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_AIRPORT_PREFERENCE',
       'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_BLOCK_REASON_LIST');

DELETE FROM atlas_dashboard.capability_endpoint
 WHERE capability_id = 'city-operations.driver.write'
   AND server_name = 'DASHBOARD'
   AND endpoint_id IN (
       'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_AIRPORT_PREFERENCE',
       'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BLOCK',
       'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BLOCK_WITH_REASON',
       'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UNBLOCK');
