











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

-- {"api":"PostMerchantConfigAllowedDestinationStates","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'city-config.launch.write' ) ON CONFLICT DO NOTHING;

-- {"api":"GetMerchantConfigAllowedDestinationStates","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'city-config.launch.read' ) ON CONFLICT DO NOTHING;
-- {"api":"PostMerchantCloudUpdate","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'system-config.merchant.write' ) ON CONFLICT DO NOTHING;
