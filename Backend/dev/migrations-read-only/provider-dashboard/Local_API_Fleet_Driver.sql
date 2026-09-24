

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

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

-- {"api":"PostDriverFleetChangeDriver","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
------- SQL updates -------

------- SQL updates -------

-- {"api":"PostDriverFleetVehicleChangeFleetOwner","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'city-operations.onboarding.write' ) ON CONFLICT DO NOTHING;
-- {"api":"PostDriverFleetCashRideUpdate","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'city-operations.driver_management.write' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetDriverFleetAccessList","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetOwnerList","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetAccessSelect","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetV2AccessSelect","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetV2AccessMultiOwnerIdSelect","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetAddVehicles","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverAddRidePayoutAccountNumber","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetAddVehicle","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetGetDriverRequests","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetRespondDriverRequest","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetAddRCWithoutDriver","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetGetAllVehicle","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetGetAllDriver","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetGetAllBadge","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetUnlink","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetRemoveVehicle","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetRemoveDriver","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetTotalEarning","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetVehicleEarning","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetDriverEarning","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetBookings","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetAssignments","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetDriverVehicleAssociation","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetDriverListStats","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetDriverAssociation","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetVehicleAssociation","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetVehicleEdit","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetVehicleDriverRcStatus","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverUpdateFleetOwnerInfo","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetOwnerInfo","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetOperatorInfo","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetSendJoiningOtp","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetVerifyJoiningOtp","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetRoutes","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetPossibleRoutes","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetTripPlanner","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetTripTransactions","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetAddDrivers","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetAddDriverBusRouteMapping","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetLinkRCWithDriver","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverDashboardFleetWmbTripEnd","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverDashboardFleetTripWaypoints","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetWmbRouteDetails","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetGetNearbyDrivers","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverDashboardFleetTrackDriver","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverDashboardInternalHelperGetFleetOwnerId","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverDashboardInternalHelperGetFleetOwnerIds","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetStatus","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetLocationList","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetGetDriverDetails","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetGetNearbyDriversV2","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetDashboardAnalyticsAllTime","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetDashboardAnalytics","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetDashboardAnalyticsCache","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverDashboardFleetEstimateRoute","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetApproveDriver","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetDriverUpdate","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetDriverChangeFleetOwner","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetDriverDetails","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetTripTransactionsV2","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetVehicleListStats","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetDriverOnboardedDriversAndUnlinkedVehicles","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetStatusSummary","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverFleetScheduledBookingList","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetScheduledBookingAssign","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetScheduledBookingCancel","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostDriverFleetScheduledBookingReassign","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetDriverVehicleInfo","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.
