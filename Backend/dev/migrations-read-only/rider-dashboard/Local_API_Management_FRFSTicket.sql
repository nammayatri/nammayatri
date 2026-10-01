-- {"api":"GetFRFSTicketFrfsRoutes","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFRFSTicketFrfsRouteFareList","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PutFRFSTicketFrfsRouteFareUpsert","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PutFRFSTicketFrfsRouteTypeUpsert","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'transit-operations.master.write' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFRFSTicketFrfsStageFareList","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'transit-operations.master.read' ) ON CONFLICT DO NOTHING;

-- {"api":"PutFRFSTicketFrfsStageFareUpsert","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'transit-operations.master.write' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFRFSTicketFrfsRouteStations","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFRFSTicketFrfsGtfs","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostFRFSTicketFrfsStatusUpdate","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.
