-- {"api":"PostFrfsFleetOperatorCurrentOperation","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'transit-operations.trip.execute' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFrfsFleetOperatorTripAction","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'transit-operations.trip.execute' ) ON CONFLICT DO NOTHING;
