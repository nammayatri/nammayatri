-- {"api":"PostOverlayCreate","migration":"endpoint","param":"OverlayAPI CreateOverlayEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/OVERLAY/POST_OVERLAY_CREATE'
  WHERE endpoint = 'OverlayAPI CreateOverlayEndpoint';

-- {"api":"PostOverlayDelete","migration":"endpoint","param":"OverlayAPI DeleteOverlayEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/OVERLAY/POST_OVERLAY_DELETE'
  WHERE endpoint = 'OverlayAPI DeleteOverlayEndpoint';

-- {"api":"PostOverlaySchedule","migration":"endpoint","param":"OverlayAPI ScheduleOverlayEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/OVERLAY/POST_OVERLAY_SCHEDULE'
  WHERE endpoint = 'OverlayAPI ScheduleOverlayEndpoint';
