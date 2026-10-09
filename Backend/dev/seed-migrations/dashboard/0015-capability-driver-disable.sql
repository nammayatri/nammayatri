-- Move the disable-driver endpoint (PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_DISABLE)
-- off city-operations.driver.write onto city-operations.driver_block.write, next to
-- block / unblock (see 0013).
--
-- The generator emits the new capability_endpoint link from the
-- `migrate: capability:` line in Management/API/Driver.yaml. The capability row
-- already exists (0013). Capability checks are ANY-of and the generator only
-- inserts, so the old driver.write link is deleted here; otherwise driver.write
-- would keep granting it.

DELETE FROM atlas_dashboard.capability_endpoint
 WHERE capability_id = 'city-operations.driver.write'
   AND server_name = 'DASHBOARD'
   AND endpoint_id = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_DISABLE';
