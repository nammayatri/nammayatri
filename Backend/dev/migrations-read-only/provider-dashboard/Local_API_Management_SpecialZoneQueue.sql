-- {"api":"PostSpecialZoneQueueTriggerNotify","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetSpecialZoneQueueTriggerNotifyStatus","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetSpecialZoneQueueQueueStats","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostSpecialZoneQueueManualQueueAdd","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostSpecialZoneQueueManualQueueRemove","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetSpecialZoneQueueDriverQueuePosition","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetSpecialZoneQueueDriverQueueHistory","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetSpecialZoneQueueDriverQueueRequests","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'city-operations.airport_queue.read' ) ON CONFLICT DO NOTHING;
