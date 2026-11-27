{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handlers for direct-dashboard routes whose request needs more
-- than the verified operator's id or name: fleet-owner resolution, fleet-owner
-- verification, dashboard-database writes after the call, and similar.
--
-- provider-dashboard did this work in its own hand-written
-- @Domain.Action.ProviderPlatform.*@ layer before forwarding the call. The
-- generated @API.Action.DashboardAuth@ handler calls these functions instead of
-- the domain handler for every endpoint marked @appServerHandler: custom@ in
-- the spec, so this logic lives here and is never overwritten by the generator.
module Domain.Action.DashboardAuth.Fleet.Driver
  ( getDriverDashboardInternalHelperGetFleetOwnerId,
    getDriverDashboardInternalHelperGetFleetOwnerIds,
    getDriverFleetOwnerInfo,
    getDriverFleetOwnerList,
    getDriverVehicleInfo,
    getDriverFleetGetAllBadge,
    getDriverFleetGetDriverRequests,
    getDriverFleetBookings,
    getDriverFleetAssignments,
    postDriverFleetCashRideUpdate,
    postDriverAddRidePayoutAccountNumber,
    postDriverFleetDashboardAnalyticsCache,
    postDriverFleetDriverChangeFleetOwner,
    getDriverFleetAccessList,
    getDriverFleetTripTransactions,
    postDriverFleetTripTransactionsV2,
    getDriverDashboardFleetTripWaypoints,
    postDriverFleetAccessSelect,
    postDriverFleetV2AccessSelect,
    postDriverFleetV2AccessMultiOwnerIdSelect,
    postDriverFleetAddVehicles,
    postDriverFleetAddVehicle,
    postDriverFleetRespondDriverRequest,
    postDriverFleetAddRCWithoutDriver,
    getDriverFleetGetAllVehicle,
    getDriverFleetGetAllDriver,
    getDriverFleetScheduledBookingList,
    postDriverFleetScheduledBookingAssign,
    postDriverFleetScheduledBookingCancel,
    postDriverFleetScheduledBookingReassign,
    postDriverFleetUnlink,
    postDriverFleetRemoveVehicle,
    postDriverFleetRemoveDriver,
    getDriverFleetTotalEarning,
    getDriverFleetVehicleEarning,
    getDriverFleetDriverEarning,
    getDriverFleetDriverVehicleAssociation,
    getDriverFleetDriverListStats,
    getDriverFleetDriverAssociation,
    getDriverFleetVehicleAssociation,
    postDriverFleetVehicleEdit,
    postDriverFleetVehicleDriverRcStatus,
    postDriverUpdateFleetOwnerInfo,
    getDriverFleetOperatorInfo,
    postDriverFleetSendJoiningOtp,
    postDriverFleetVerifyJoiningOtp,
    getDriverFleetRoutes,
    getDriverFleetPossibleRoutes,
    postDriverFleetTripPlanner,
    postDriverFleetAddDrivers,
    postDriverFleetAddDriverBusRouteMapping,
    postDriverFleetLinkRCWithDriver,
    postDriverDashboardFleetWmbTripEnd,
    getDriverFleetWmbRouteDetails,
    postDriverFleetGetNearbyDrivers,
    postDriverDashboardFleetTrackDriver,
    getDriverFleetStatus,
    postDriverFleetGetNearbyDriversV2,
    getDriverFleetDashboardAnalyticsAllTime,
    getDriverFleetDashboardAnalytics,
    postDriverDashboardFleetEstimateRoute,
    postDriverFleetApproveDriver,
    postDriverFleetOnboardingLinkConsent,
    postDriverFleetDriverUpdate,
    getDriverFleetVehicleListStats,
    getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles,
    getDriverFleetStatusSummary,
    getDriverFleetDriverDetails,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2
import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import qualified Data.Time
import qualified Domain.Action.Dashboard.Fleet.Access
import qualified Domain.Action.Dashboard.Fleet.Driver
import qualified Domain.Types.Alert.AlertRequestStatus
import qualified Domain.Types.Alert.AlertRequestType
import qualified Domain.Types.FleetBadgeType
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import Kernel.Beam.Functions (runInDashboardDb)
import qualified Kernel.External.Maps
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.Fleet
import qualified SharedLogic.FleetScope
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import Tools.Auth
import qualified Tools.Auth.DashboardRegistration
import Tools.Auth.DashboardUserAuth
import qualified "lib-dashboard" Tools.Auth.FleetScope as LibFleetScope

-- | The old per-fleet on/off toggles. Once a merchant enforces scoped assignments they no longer touch
-- the shared member row: they change this session's own fleet selection, the same thing
-- @POST /user/switchScope@ does. Before that they behave as they always did, for the caller's own
-- member id (the id in the request is not trusted).
toggleSessionFleets :: DashboardUser -> [Kernel.Prelude.Text] -> Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Environment.Flow Kernel.Types.APISuccess.APISuccess
toggleSessionFleets user fleetIds enable onlyCurrent = do
  runInDashboardDb $ LibFleetScope.toggleFleetSelection user.sessionToken user.personId user.merchant.id fleetIds enable onlyCurrent
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success

postDriverFleetAccessSelect :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetAccessSelect a7 a6 a5 a4 _a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a5 = toggleSessionFleets a5 [a4] a1 (Kernel.Prelude.fromMaybe Kernel.Prelude.False a2)
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAccessSelect a7 a6 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5)) a2 a1

postDriverFleetV2AccessSelect :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetV2AccessSelect a8 a7 a6 _a5 a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a6 = case a4 of
    Kernel.Prelude.Just fleetOwnerId -> toggleSessionFleets a6 [fleetOwnerId] a1 (Kernel.Prelude.fromMaybe Kernel.Prelude.False a2)
    Kernel.Prelude.Nothing -> Kernel.Utils.Common.throwError (Kernel.Types.Error.InvalidRequest "Name the fleet to select; group codes are not supported once fleet scope is enforced")
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Fleet.Driver.postDriverFleetV2AccessSelect a8 a7 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a6)) a4 a3 a2 a1

postDriverFleetV2AccessMultiOwnerIdSelect :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> API.Types.ProviderPlatform.Fleet.Driver.MultiOwnerSelect -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetV2AccessMultiOwnerIdSelect a7 a6 a5 _a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a5 = toggleSessionFleets a5 a1.fleetOwnerIds a2 (Kernel.Prelude.fromMaybe Kernel.Prelude.False a3)
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Fleet.Driver.postDriverFleetV2AccessMultiOwnerIdSelect a7 a6 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5)) a3 a2 a1

postDriverFleetAddVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.CreateVehiclesReq -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.APISuccessWithUnprocessedEntities)
postDriverFleetAddVehicles a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a3
  (mbFleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedMbFleetOwnerAndRequestorId a3 a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddVehicles a5 a4 a1 {API.Types.ProviderPlatform.Fleet.Driver.fleetOwnerId = mbFleetOwnerId, API.Types.ProviderPlatform.Fleet.Driver.requestorId = Kernel.Prelude.Just requestorId}

postDriverFleetAddVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Dashboard.Common.Role) -> API.Types.ProviderPlatform.Fleet.Driver.AddVehicleReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetAddVehicle a8 a7 a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a6
  (mbFleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedMbFleetOwnerAndRequestorId a6 a3
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddVehicle a8 a7 a5 requestorId mbFleetOwnerId a4 a2 a1

postDriverFleetRespondDriverRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.RequestRespondReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetRespondDriverRequest a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a3 a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetRespondDriverRequest a5 a4 fleetOwnerId a1

postDriverFleetAddRCWithoutDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Dashboard.ProviderPlatform.Management.DriverRegistration.RegisterRCReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetAddRCWithoutDriver a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a3
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a3 a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddRCWithoutDriver a5 a4 fleetOwnerId a1

-- | The page for a caller whose lists cover no fleet at all: an operator with none linked.
noListedFleets :: DashboardUser -> Dashboard.Common.DriverSummary -> API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationResT
noListedFleets dashboardUser summary =
  API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationResT
    { fleetOwnerId = Tools.Auth.DashboardUserAuth.dashboardRequestorId dashboardUser,
      listItem = [],
      summary
    }

-- | Whether a list refuses to open when one of its fleets is disabled, where the merchant checks that. An
-- operator's list of all their fleets does not: one disabled fleet must not hide the others, and this list
-- never made that check before it was limited to their fleets.
checksEachListedFleet :: DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Bool
checksEachListedFleet dashboardUser mbFleetOwnerId =
  Kernel.Prelude.not (SharedLogic.FleetScope.isEnforcedOperator dashboardUser) Kernel.Prelude.|| Kernel.Prelude.isJust mbFleetOwnerId

-- | The fleet a list request may name. The request used to be trusted as it came, so any caller
-- could read any fleet by naming it. A fleet owner or operator may only name a fleet they own,
-- belong to or operate; every other role is authorised by capability and still names any fleet.
-- This only decides who may name what. Whether the fleet is in the caller's scope is checked by
-- 'authorizeFleetIds', which the callers reach through 'scopedFleetOwnerIds' or 'enforcedFleetOwnersInfo'.
resolveListedFleetOwner :: DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow (Kernel.Prelude.Maybe Kernel.Prelude.Text)
resolveListedFleetOwner dashboardUser mbFleetOwnerId
  | not (Tools.Auth.DashboardUserAuth.requestorIsFleetSide dashboardUser) = Kernel.Prelude.pure mbFleetOwnerId
  | otherwise =
    Kernel.Prelude.forM mbFleetOwnerId $ \fleetOwnerId ->
      if Tools.Auth.DashboardUserAuth.requestorIsFleetOwner dashboardUser
        then -- Once fleet scope is enforced the fleets a person may name are the ones in their scope, which
        -- 'authorizeFleetIds' checks in the caller; the fleet member association is not consulted.

          if SharedLogic.FleetScope.isEnforced dashboardUser Kernel.Prelude.|| fleetOwnerId Kernel.Prelude.== Tools.Auth.DashboardUserAuth.dashboardRequestorId dashboardUser
            then Kernel.Prelude.pure fleetOwnerId
            else SharedLogic.Fleet.verifyFleetOwnerAccess (Tools.Auth.DashboardUserAuth.dashboardRequestorId dashboardUser) fleetOwnerId
        else do
          void $ Domain.Action.Dashboard.Fleet.Access.checkRequestorAccessToFleet Kernel.Prelude.False (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId dashboardUser)) fleetOwnerId
          Kernel.Prelude.pure fleetOwnerId

getDriverFleetGetAllVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.Driver.ApprovalStatusFilter) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.ListVehicleResT)
getDriverFleetGetAllVehicle a14 a13 a12 a11 a10 a9 a8 a7 _a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a12
  cities <- SharedLogic.FleetScope.reachableCities a12 a14.getShortId a13 a1
  mbFleetOwnerId <- resolveListedFleetOwner a12 a8
  -- The fleets come from the caller's scope: the one the request names, or all the ones selected.
  scoped <- SharedLogic.FleetScope.scopedFleetOwnerIds a12 mbFleetOwnerId
  fleets <- SharedLogic.FleetScope.leaveOutDisabledFleets a12 a14 Kernel.Prelude.fst scoped
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetGetAllVehicleForFleets a14 a13 cities a11 a10 a9 a7 a5 a4 a3 a2 fleets

getDriverFleetGetAllDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.Driver.ApprovalStatusFilter) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.Driver.OnboardingAs) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetListDriverResT)
getDriverFleetGetAllDriver a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 _a8 a7 a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a16
  cities <- SharedLogic.FleetScope.reachableCities a16 a18.getShortId a17 a1
  mbFleetOwnerId <- resolveListedFleetOwner a16 a10
  scoped <- SharedLogic.FleetScope.scopedFleetOwnerIds a16 mbFleetOwnerId
  fleets <- SharedLogic.FleetScope.leaveOutDisabledFleets a16 a18 Kernel.Prelude.fst scoped
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetGetAllDriverForFleets a18 a17 a15 a14 a13 a12 a11 a9 a7 a6 a5 a4 a3 a2 cities fleets

postDriverFleetUnlink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetUnlink a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a4
  (mbFleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedMbFleetOwnerAndRequestorId a4 a1
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetUnlink a6 a5 requestorId a3 a2 mbFleetOwnerId

postDriverFleetRemoveVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetRemoveVehicle a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a3
  (fleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedFleetOwnerAndRequestorId a3 a1
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetRemoveVehicle a5 a4 fleetOwnerId a2 (Kernel.Prelude.Just requestorId)

postDriverFleetRemoveDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetRemoveDriver a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a4
  (mbFleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedMbFleetOwnerAndRequestorId a4 a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetRemoveDriver a6 a5 requestorId a3 mbFleetOwnerId a1

getDriverFleetTotalEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetTotalEarningResponse)
getDriverFleetTotalEarning a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a3 Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetTotalEarning a5 a4 fleetOwnerId a2 a1

getDriverFleetVehicleEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes)
getDriverFleetVehicleEarning a8 a7 a6 a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a6 Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetVehicleEarning a8 a7 fleetOwnerId a5 a4 a3 a2 a1

getDriverFleetDriverEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.SortOn) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes)
getDriverFleetDriverEarning a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a9 Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverEarning a11 a10 fleetOwnerId a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetDriverVehicleAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes)
getDriverFleetDriverVehicleAssociation a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a9
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a9 Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverVehicleAssociation a11 a10 fleetOwnerId a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetDriverListStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.FleetDriverListStatsSortOn) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.FleetDriverStatsResponseType) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetDriverStatsListRes)
getDriverFleetDriverListStats a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  (mbFleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedMbFleetOwnerAndRequestorId a12 a9
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverListStats a14 a13 requestorId a11 a10 mbFleetOwnerId a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetDriverAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.DriverMode) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.DocsVerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationResT)
getDriverFleetDriverAssociation a26 a25 a24 a23 a22 a21 a20 a19 a18 a17 a16 a15 a14 a13 a12 _a11 _a10 _a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a24
  cities <- SharedLogic.FleetScope.reachableCities a24 a26.getShortId a25 a1
  mbFleetOwnerId <- resolveListedFleetOwner a24 a12
  mbEnforcedFleets <- SharedLogic.FleetScope.enforcedFleetOwnersInfo a24 mbFleetOwnerId
  case mbEnforcedFleets of
    Kernel.Prelude.Just [] -> Kernel.Prelude.pure (noListedFleets a24 (Dashboard.Common.DriverSummary 0 0 0 0))
    Kernel.Prelude.Just fleets -> do
      listed <- SharedLogic.FleetScope.leaveOutDisabledFleets a24 a26 (.fleetOwnerId) fleets
      Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverAssociationForFleets (checksEachListedFleet a24 mbFleetOwnerId) a26 a25 cities a23 a22 a21 a20 a19 a18 a17 a16 a15 a14 a13 a8 a7 a6 a5 a4 a3 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a24) listed
    Kernel.Prelude.Nothing -> do
      -- Not enforced: the old path runs, and here is where SHADOW notices a named fleet the assignments do not cover.
      SharedLogic.FleetScope.authorizeFleetIds a24 (Kernel.Prelude.maybeToList mbFleetOwnerId)
      Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverAssociation a26 a25 a23 a22 a21 a20 a19 a18 a17 a16 a15 a14 a13 mbFleetOwnerId (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a24)) (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a24) (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a24)) a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetVehicleAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.FleetVehicleStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.DocsVerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationResT)
getDriverFleetVehicleAssociation a20 a19 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 _a7 _a6 _a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a18
  cities <- SharedLogic.FleetScope.reachableCities a18 a20.getShortId a19 a1
  mbFleetOwnerId <- resolveListedFleetOwner a18 a8
  mbEnforcedFleets <- SharedLogic.FleetScope.enforcedFleetOwnersInfo a18 mbFleetOwnerId
  case mbEnforcedFleets of
    Kernel.Prelude.Just [] -> Kernel.Prelude.pure (noListedFleets a18 (Dashboard.Common.DriverSummary 0 0 0 0))
    Kernel.Prelude.Just fleets -> do
      listed <- SharedLogic.FleetScope.leaveOutDisabledFleets a18 a20 (.fleetOwnerId) fleets
      Domain.Action.Dashboard.Fleet.Driver.getDriverFleetVehicleAssociationForFleets (checksEachListedFleet a18 mbFleetOwnerId) a20 a19 cities a17 a16 a15 a14 a13 a12 a11 a10 a9 a4 a3 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a18) listed
    Kernel.Prelude.Nothing -> do
      -- Not enforced: the old path runs, and here is where SHADOW notices a named fleet the assignments do not cover.
      SharedLogic.FleetScope.authorizeFleetIds a18 (Kernel.Prelude.maybeToList mbFleetOwnerId)
      Domain.Action.Dashboard.Fleet.Driver.getDriverFleetVehicleAssociation a20 a19 a17 a16 a15 a14 a13 a12 a11 a10 a9 mbFleetOwnerId (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a18)) (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a18) (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a18)) a4 a3 a2 a1

postDriverFleetVehicleEdit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.EditVehicleReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetVehicleEdit a8 a7 a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a6
  (mbFleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedMbFleetOwnerAndRequestorId a6 a5
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetVehicleEdit a8 a7 requestorId mbFleetOwnerId a4 a3 a2 a1

postDriverFleetVehicleDriverRcStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.RCStatusReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetVehicleDriverRcStatus a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a4
  (mbFleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedMbFleetOwnerAndRequestorId a4 a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetVehicleDriverRcStatus a6 a5 a3 requestorId mbFleetOwnerId a1

postDriverUpdateFleetOwnerInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.UpdateFleetOwnerInfoReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverUpdateFleetOwnerInfo a6 a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a4 a2
  Kernel.Prelude.unless (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4 Kernel.Prelude.== a3.getId Kernel.Prelude.&& fleetOwnerId Kernel.Prelude.== Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) $
    Kernel.Utils.Common.throwError Kernel.Types.Error.AccessDenied
  Domain.Action.Dashboard.Fleet.Driver.postDriverUpdateFleetOwnerInfo a6 a5 (Kernel.Types.Id.Id fleetOwnerId) a1

getDriverFleetOperatorInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetOwnerInfoRes)
getDriverFleetOperatorInfo a7 a6 _a5 a4 a3 a2 a1 = do
  res <- Domain.Action.Dashboard.Fleet.Driver.getDriverFleetOperatorInfo a7 a6 a4 a3 a2 a1
  mbApprovedBy <- Tools.Auth.DashboardRegistration.dashboardPersonApprovedBy res.id
  Kernel.Prelude.pure res {API.Types.ProviderPlatform.Fleet.Driver.approvedBy = maybe res.approvedBy Kernel.Prelude.Just mbApprovedBy}

postDriverFleetSendJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> Environment.Flow Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes)
postDriverFleetSendJoiningOtp a5 a4 a3 a2 a1 = do
  (fleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedFleetOwnerAndRequestorId a3 a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetSendJoiningOtp a5 a4 (Tools.Auth.DashboardUserAuth.dashboardRequestorName a3) (Kernel.Prelude.Just fleetOwnerId) (Kernel.Prelude.Just requestorId) a1

postDriverFleetVerifyJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.VerifyFleetJoiningOtpReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetVerifyJoiningOtp a6 a5 a4 a3 a2 a1 = do
  (fleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedFleetOwnerAndRequestorId a4 a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetVerifyJoiningOtp a6 a5 fleetOwnerId a3 (Kernel.Prelude.Just requestorId) a1

getDriverFleetRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.External.Maps.Types.LatLong) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.RouteAPIResp)
getDriverFleetRoutes a8 a7 a6 a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a6 a3
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetRoutes a8 a7 fleetOwnerId a5 a4 a2 a1

getDriverFleetPossibleRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.RouteAPIResp)
getDriverFleetPossibleRoutes a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a3 a2
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetPossibleRoutes a5 a4 fleetOwnerId a1

postDriverFleetTripPlanner :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.TripPlannerReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetTripPlanner a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a3 a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetTripPlanner a5 a4 fleetOwnerId a1

postDriverFleetAddDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.CreateDriversReq -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.AddDriversResp)
postDriverFleetAddDrivers a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a3
  (mbFleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedMbFleetOwnerAndRequestorId a3 a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddDrivers a5 a4 (Kernel.Prelude.Just requestorId) a1 {API.Types.ProviderPlatform.Fleet.Driver.fleetOwnerId = mbFleetOwnerId}

postDriverFleetAddDriverBusRouteMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.CreateDriverBusRouteMappingReq -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.APISuccessWithUnprocessedEntities)
postDriverFleetAddDriverBusRouteMapping a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a3 a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddDriverBusRouteMapping a5 a4 a1 {API.Types.ProviderPlatform.Fleet.Driver.fleetOwnerId = Kernel.Prelude.Just fleetOwnerId}

postDriverFleetLinkRCWithDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.LinkRCWithDriverForFleetReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetLinkRCWithDriver a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a3
  (fleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedFleetOwnerAndRequestorId a3 a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetLinkRCWithDriver a5 a4 fleetOwnerId (Kernel.Prelude.Just requestorId) a1

postDriverDashboardFleetWmbTripEnd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.TripTransaction -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Dashboard.Common.ActionSource) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverDashboardFleetWmbTripEnd a6 a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a4 a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverDashboardFleetWmbTripEnd a6 a5 a3 fleetOwnerId a1

getDriverFleetWmbRouteDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.RouteDetails)
getDriverFleetWmbRouteDetails a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a3 a1
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetWmbRouteDetails a5 a4 fleetOwnerId a2

postDriverFleetGetNearbyDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.NearbyDriverReq -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.NearbyDriverRespT)
postDriverFleetGetNearbyDrivers a4 a3 a2 a1 = do
  fleetOwnerIds <- SharedLogic.FleetScope.scopedFleetOwnerIds a2 Kernel.Prelude.Nothing
  drivers <-
    concatMapM
      ( \(fleetOwnerId, fleetOwnerName) -> do
          API.Types.ProviderPlatform.Fleet.Driver.NearbyDriverResp {..} <- Domain.Action.Dashboard.Fleet.Driver.postDriverFleetGetNearbyDrivers a4 a3 fleetOwnerId a1
          Kernel.Prelude.pure $ Kernel.Prelude.map (\API.Types.ProviderPlatform.Fleet.Driver.DriverInfo {..} -> API.Types.ProviderPlatform.Fleet.Driver.DriverInfoT {..}) drivers
      )
      fleetOwnerIds
  Kernel.Prelude.pure API.Types.ProviderPlatform.Fleet.Driver.NearbyDriverRespT {..}

postDriverDashboardFleetTrackDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.TrackDriverLocationsReq -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.TrackDriverLocationsRes)
postDriverDashboardFleetTrackDriver a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a3 a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverDashboardFleetTrackDriver a5 a4 fleetOwnerId a1

getDriverFleetStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.DriverStatusRes)
getDriverFleetStatus a4 a3 a2 a1 = do
  (mbFleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedMbFleetOwnerAndRequestorId a2 a1
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetStatus a4 a3 requestorId mbFleetOwnerId

postDriverFleetGetNearbyDriversV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.NearbyDriversReqV2 -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.NearbyDriversRespTV2)
postDriverFleetGetNearbyDriversV2 a4 a3 a2 a1 = do
  fleetOwnerIds <- SharedLogic.FleetScope.scopedFleetOwnerIds a2 Kernel.Prelude.Nothing
  drivers <-
    concatMapM
      ( \(fleetOwnerId, fleetOwnerName) -> do
          API.Types.ProviderPlatform.Fleet.Driver.NearbyDriversRespV2 {..} <- Domain.Action.Dashboard.Fleet.Driver.postDriverFleetGetNearbyDriversV2 a4 a3 fleetOwnerId a1
          Kernel.Prelude.pure $ Kernel.Prelude.map (\API.Types.ProviderPlatform.Fleet.Driver.NearbyDriverDetails {..} -> API.Types.ProviderPlatform.Fleet.Driver.NearbyDriverDetailsT {..}) drivers
      )
      fleetOwnerIds
  Kernel.Prelude.pure API.Types.ProviderPlatform.Fleet.Driver.NearbyDriversRespTV2 {..}

getDriverFleetDashboardAnalyticsAllTime :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.AllTimeFleetAnalyticsRes)
getDriverFleetDashboardAnalyticsAllTime a4 a3 a2 a1 = do
  (fleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedFleetOwnerAndRequestorId a2 a1
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDashboardAnalyticsAllTime a4 a3 fleetOwnerId (Kernel.Prelude.Just requestorId)

getDriverFleetDashboardAnalytics :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.FleetAnalyticsResponseType) -> Data.Time.Day -> Data.Time.Day -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetAnalyticsRes)
getDriverFleetDashboardAnalytics a7 a6 a5 a4 a3 a2 a1 = do
  (fleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedFleetOwnerAndRequestorId a5 a4
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDashboardAnalytics a7 a6 fleetOwnerId (Kernel.Prelude.Just requestorId) a3 a2 a1

postDriverDashboardFleetEstimateRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.EstimateRouteReq -> Environment.Flow Kernel.External.Maps.GetRoutesResp)
postDriverDashboardFleetEstimateRoute a5 a4 a3 _a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a3 Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.postDriverDashboardFleetEstimateRoute a5 a4 fleetOwnerId a1

postDriverFleetApproveDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.ApproveDriverReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetApproveDriver a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a2 Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetApproveDriver a4 a3 fleetOwnerId a1

postDriverFleetOnboardingLinkConsent :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.OnboardingLinkConsentReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetOnboardingLinkConsent a5 a4 a3 a2 a1 = do
  (fleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedFleetOwnerAndRequestorId a3 a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetOnboardingLinkConsent a5 a4 fleetOwnerId (Kernel.Prelude.Just requestorId) a1

postDriverFleetDriverUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Fleet.Driver.UpdateDriverReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetDriverUpdate a5 a4 a3 a2 a1 = do
  -- Once fleet scope is enforced the caller acts as the fleet in their scope that holds the subject, which need not
  -- be the one they signed in as. Anyone else's driver is refused here.
  requestorId <-
    if SharedLogic.FleetScope.isEnforced a3
      then SharedLogic.FleetScope.subjectActingFleet a3 a2.getId
      else Kernel.Prelude.pure (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)
  Kernel.Prelude.whenJust a1.email $ \email -> Tools.Auth.DashboardRegistration.assertDashboardEmailAvailable email a2.getId
  res <- Domain.Action.Dashboard.Fleet.Driver.postDriverFleetDriverUpdate a5 a4 a2 requestorId a1
  Tools.Auth.DashboardRegistration.updateDashboardPersonProfile a2.getId a1.firstName a1.lastName a1.email a1.mobileNo a1.mobileCountryCode
  Kernel.Prelude.pure res

getDriverFleetVehicleListStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Data.Time.Day -> Data.Time.Day -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetVehicleStatsRes)
getDriverFleetVehicleListStats a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  (fleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedFleetOwnerAndRequestorId a7 a6
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetVehicleListStats a9 a8 fleetOwnerId (Kernel.Prelude.Just requestorId) a5 a4 a3 a2 a1

getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.OnboardedDriversAndUnlinkedVehiclesRes)
getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles a6 a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a4 (Kernel.Prelude.Just a3)
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles a6 a5 fleetOwnerId a2 a1

getDriverFleetStatusSummary :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.EntityOperationType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.StatusSummaryResponse)
getDriverFleetStatusSummary a6 a5 a4 a3 a2 a1 = do
  cities <- SharedLogic.FleetScope.reachableCities a4 a6.getShortId a5 a1
  -- The fleet owners' own counts are told apart by the city alone, so they keep the single-fleet path.
  mbEnforcedFleets <- case a3 of
    API.Types.ProviderPlatform.Fleet.Driver.FLEET_OWNER_STATUS -> Kernel.Prelude.pure Kernel.Prelude.Nothing
    _ -> SharedLogic.FleetScope.enforcedFleetOwnersInfo a4 a2
  case mbEnforcedFleets of
    Kernel.Prelude.Just [] -> Kernel.Prelude.pure (API.Types.ProviderPlatform.Fleet.Driver.StatusSummaryResponse {total = 0, verified = 0, approved = 0, pending = 0, rejected = 0})
    Kernel.Prelude.Just fleets -> Domain.Action.Dashboard.Fleet.Driver.getDriverFleetStatusSummaryForFleets a6 a5 cities a3 (Kernel.Prelude.map (.fleetOwnerId) fleets)
    Kernel.Prelude.Nothing -> do
      (mbFleetOwnerId, _) <- SharedLogic.FleetScope.scopedMbFleetOwnerAndRequestorId a4 a2
      Domain.Action.Dashboard.Fleet.Driver.getDriverFleetStatusSummary a6 a5 a3 mbFleetOwnerId cities

-- The scheduled-booking endpoints act for a fleet. The generated handler used the logged-in person's
-- id as the fleet's id, which is only true for an owner on their own: a member who works for two
-- fleets, or an admin, would act for the wrong one. The fleet now comes from the caller's scope.

getDriverFleetScheduledBookingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Data.Time.Day -> Kernel.Prelude.Maybe Data.Time.Day -> Kernel.Prelude.Maybe Dashboard.Common.TripCategory -> Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetScheduledBookingListRes)
getDriverFleetScheduledBookingList a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  -- The list is not per fleet (the fleet id is not used), so a caller holding several fleets is not asked to choose one;
  -- they only have to act for at least one.
  fleetOwnerId <-
    if SharedLogic.FleetScope.isEnforced a7
      then Kernel.Prelude.maybe "" Kernel.Prelude.fst Kernel.Prelude.. Kernel.Prelude.listToMaybe Kernel.Prelude.<$> SharedLogic.FleetScope.scopedFleetOwnerIds a7 Kernel.Prelude.Nothing
      else Kernel.Prelude.pure (Tools.Auth.DashboardUserAuth.dashboardRequestorId a7)
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetScheduledBookingList a9 a8 fleetOwnerId a6 a5 a4 a3 a2 a1

postDriverFleetScheduledBookingAssign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.AssignScheduledBookingReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetScheduledBookingAssign a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.callerOrScopedFleetOwnerId a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetScheduledBookingAssign a4 a3 fleetOwnerId a1

postDriverFleetScheduledBookingCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.CancelScheduledBookingReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetScheduledBookingCancel a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.callerOrScopedFleetOwnerId a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetScheduledBookingCancel a4 a3 fleetOwnerId a1

postDriverFleetScheduledBookingReassign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.ReassignScheduledBookingReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetScheduledBookingReassign a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.FleetScope.callerOrScopedFleetOwnerId a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetScheduledBookingReassign a4 a3 fleetOwnerId a1

-- | The member id these routes act for is the caller's own, never one the request names; a fleet the request
-- names has to be one the caller's scope holds. provider-dashboard did the same before it forwarded the call.
getDriverFleetAccessList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetOwnerListRes)
getDriverFleetAccessList a4 a3 a2 _a1
  | SharedLogic.FleetScope.isEnforced a2 = SharedLogic.FleetScope.heldFleetOwnerList a2
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Fleet.Driver.getDriverFleetAccessList a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2))

getDriverFleetTripTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.TripTransactionRespT)
getDriverFleetTripTransactions a11 a10 a9 a8 a7 a6 a5 a4 _a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a9 = do
    fleets <- SharedLogic.FleetScope.enforcedSingleFleet a9 a4
    Domain.Action.Dashboard.Fleet.Driver.getDriverFleetTripTransactionsResolving (Kernel.Prelude.const (Kernel.Prelude.pure fleets)) a11 a10 a8 a7 a6 a5 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a9)) a2 a1
  | Kernel.Prelude.otherwise = do
    (mbFleetOwnerId, requestorId) <- SharedLogic.FleetScope.passThroughFleetAndRequestor a9 a4
    Domain.Action.Dashboard.Fleet.Driver.getDriverFleetTripTransactions a11 a10 a8 a7 a6 a5 mbFleetOwnerId (Kernel.Prelude.Just requestorId) a2 a1

postDriverFleetTripTransactionsV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Driver.TripStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.TripTransactionRespT)
postDriverFleetTripTransactionsV2 a13 a12 a11 a10 a9 a8 a7 _a6 a5 a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a11 = do
    fleets <- SharedLogic.FleetScope.enforcedSingleFleet a11 a7
    Domain.Action.Dashboard.Fleet.Driver.postDriverFleetTripTransactionsV2Resolving (Kernel.Prelude.const (Kernel.Prelude.pure fleets)) a13 a12 a10 a9 a8 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a11)) a5 a4 a3 a2 a1
  | Kernel.Prelude.otherwise = do
    (mbFleetOwnerId, requestorId) <- SharedLogic.FleetScope.passThroughFleetAndRequestor a11 a7
    Domain.Action.Dashboard.Fleet.Driver.postDriverFleetTripTransactionsV2 a13 a12 a10 a9 a8 mbFleetOwnerId (Kernel.Prelude.Just requestorId) a5 a4 a3 a2 a1

getDriverDashboardFleetTripWaypoints :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.TripTransaction -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.TripTransactionWaypointsRes)
getDriverDashboardFleetTripWaypoints a7 a6 a5 a4 a3 a2 _a1 = do
  -- The fleet named has to be one the caller may act for.
  fleetOwnerId <- SharedLogic.FleetScope.scopedFleetOwnerId a5 (Kernel.Prelude.Just a3)
  Domain.Action.Dashboard.Fleet.Driver.getDriverDashboardFleetTripWaypoints a7 a6 a4 fleetOwnerId a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5))

-- | What a request reaches by an id it names: a fleet owner, a driver, a certificate. A fleet owner may reach
-- only what their own fleets hold ('SharedLogic.FleetScope'), and the member a request acts for is the
-- caller's, as provider-dashboard made it before it forwarded the call.
getDriverFleetOwnerInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetOwnerInfoRes)
getDriverFleetOwnerInfo a4 a3 a2 a1 = do
  SharedLogic.FleetScope.assertFleetInScope a2 (Kernel.Types.Id.getId a1)
  SharedLogic.FleetScope.assertOperatorMayReadProfile a2 (Kernel.Types.Id.getId a1)
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetOwnerInfo a4 a3 a1

getDriverFleetOwnerList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Dashboard.Common.Driver.ApprovalStatusFilter -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Dashboard.Common.DocsVerificationStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetType -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.Flow [API.Types.ProviderPlatform.Fleet.Driver.FleetOwnerListItem])
getDriverFleetOwnerList a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  res <- Domain.Action.Dashboard.Fleet.Driver.getDriverFleetOwnerList a15 a14 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1
  SharedLogic.FleetScope.restrictListToCaller a13 (Kernel.Types.Id.getId . (.fleetOwnerId)) res

getDriverVehicleInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.VehicleInfo)
getDriverVehicleInfo a5 a4 a3 a2 a1 = do
  SharedLogic.FleetScope.assertRcInScope a3 a1 a2
  Domain.Action.Dashboard.Fleet.Driver.getDriverVehicleInfoInCity (SharedLogic.FleetScope.isEnforced a3) a5 a4 a2 a1

getDriverFleetGetAllBadge :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.FleetBadgeType.FleetBadgeType -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetBadgeResT)
getDriverFleetGetAllBadge a10 a9 a8 a7 a6 a5 a4 a3 a2 _a1
  | SharedLogic.FleetScope.isEnforced a8 = do
    fleets <- SharedLogic.FleetScope.enforcedSingleFleet a8 a4
    Domain.Action.Dashboard.Fleet.Driver.getDriverFleetGetAllBadgeResolving (Kernel.Prelude.const (Kernel.Prelude.pure fleets)) Kernel.Prelude.True a10 a9 a7 a6 a5 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a8))
  | Kernel.Prelude.otherwise = do
    (mbFleetOwnerId, requestorId) <- SharedLogic.FleetScope.passThroughFleetAndRequestor a8 a4
    Domain.Action.Dashboard.Fleet.Driver.getDriverFleetGetAllBadge a10 a9 a7 a6 a5 mbFleetOwnerId a3 a2 (Kernel.Prelude.Just requestorId)

getDriverFleetGetDriverRequests :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Domain.Types.Alert.AlertRequestType.AlertRequestType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.Alert.AlertRequestStatus.AlertRequestStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.DriverRequestRespT)
getDriverFleetGetDriverRequests a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  -- Once fleet scope is enforced the requests are those of the fleets in the caller's scope (or the one the request names).
  case SharedLogic.FleetScope.isEnforced a11 of
    Kernel.Prelude.True -> do
      fleets <- SharedLogic.FleetScope.scopedFleetOwnerIds a11 a4
      Domain.Action.Dashboard.Fleet.Driver.getDriverFleetGetDriverRequestsForFleets a13 a12 a10 a9 a8 a7 a6 a5 a3 a2 a1 fleets
    Kernel.Prelude.False -> Domain.Action.Dashboard.Fleet.Driver.getDriverFleetGetDriverRequests a13 a12 a10 a9 a8 a7 a6 a5 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a11)) a3 a2 a1

postDriverAddRidePayoutAccountNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.AddRidePayoutAccountNumberReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverAddRidePayoutAccountNumber a4 a3 a2 a1 = do
  SharedLogic.FleetScope.assertRcInScope a2 Kernel.Prelude.Nothing (Kernel.Prelude.Just a1.vehicleRegistrationNumber)
  Domain.Action.Dashboard.Fleet.Driver.postDriverAddRidePayoutAccountNumber a4 a3 a1

postDriverFleetDashboardAnalyticsCache :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.FleetDashboardAnalyticsCacheReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetDashboardAnalyticsCache a4 a3 a2 a1 = do
  SharedLogic.FleetScope.assertFleetInScope a2 a1.fleetOwnerId
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetDashboardAnalyticsCache a4 a3 a1

postDriverFleetDriverChangeFleetOwner :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Fleet.Driver.ChangeFleetOwnerReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetDriverChangeFleetOwner a5 a4 a3 a2 a1 = do
  SharedLogic.FleetScope.assertDriverInScope a3 (Kernel.Types.Id.cast a2)
  SharedLogic.FleetScope.assertFleetInScope a3 a1.newFleetOwnerId
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetDriverChangeFleetOwner a5 a4 a2 a1

-- | These two exist for provider-dashboard, which calls them on the token-guarded tree. They take no session,
-- so served here they would answer to anyone who asks; they are refused.
getDriverDashboardInternalHelperGetFleetOwnerId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.Flow Kernel.Prelude.Text)
getDriverDashboardInternalHelperGetFleetOwnerId _ _ _ _ = Kernel.Utils.Common.throwError Kernel.Types.Error.AccessDenied

getDriverDashboardInternalHelperGetFleetOwnerIds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.Flow [(Kernel.Prelude.Text, Kernel.Prelude.Text)])
getDriverDashboardInternalHelperGetFleetOwnerIds _ _ _ _ = Kernel.Utils.Common.throwError Kernel.Types.Error.AccessDenied

-- | A driver is read through the fleet that holds them. The route used to ask about the caller's own id, which is
-- the wrong fleet for an owner holding several, so an enforced owner is checked against each fleet they hold. A driver
-- none of them holds is left to the route, which refuses it.
getDriverFleetDriverDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.DriverDetailsRes)
getDriverFleetDriverDetails a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a2 = do
    fleetIds <- Kernel.Prelude.map Kernel.Prelude.fst Kernel.Prelude.<$> SharedLogic.FleetScope.scopedFleetOwnerIds a2 Kernel.Prelude.Nothing
    holding <- Kernel.Prelude.filterM (\fleetId -> Kernel.Prelude.isJust Kernel.Prelude.<$> QFDA.findByDriverIdAndFleetOwnerId (Kernel.Types.Id.cast a1) fleetId Kernel.Prelude.True) fleetIds
    Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverDetails a4 a3 (Kernel.Prelude.fromMaybe (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) (Kernel.Prelude.listToMaybe holding)) a1
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverDetails a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

-- The three routes below act for the fleets the caller holds. Once fleet scope is enforced they take them from the
-- caller's scope, not from the dashboard person's own id (which is one fleet at most); otherwise they behave as before.
getDriverFleetBookings :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetBookingsInformationResponse)
getDriverFleetBookings a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a9 = do
    fleets <- SharedLogic.FleetScope.scopedFleetOwnerIds a9 Kernel.Prelude.Nothing
    Domain.Action.Dashboard.Fleet.Driver.getDriverFleetBookingsForFleets a8 a7 a6 a5 a4 a3 a2 a1 fleets
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Fleet.Driver.getDriverFleetBookings a11 a10 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a9) a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetAssignments :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetBookingAssignmentsResponse)
getDriverFleetAssignments a10 a9 a8 a7 a6 a5 a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a8 = do
    fleets <- SharedLogic.FleetScope.scopedFleetOwnerIds a8 Kernel.Prelude.Nothing
    Domain.Action.Dashboard.Fleet.Driver.getDriverFleetAssignmentsForFleets a7 a6 a5 a4 a3 a2 a1 fleets
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Fleet.Driver.getDriverFleetAssignments a10 a9 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a8) a7 a6 a5 a4 a3 a2 a1

postDriverFleetCashRideUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.Driver.UpdateCashRideReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetCashRideUpdate a5 a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a3 = do
    (fleetOwnerId, requestorId) <- SharedLogic.FleetScope.scopedFleetOwnerAndRequestorId a3 a2
    Domain.Action.Dashboard.Fleet.Driver.postDriverFleetCashRideUpdate a5 a4 requestorId (Kernel.Prelude.Just fleetOwnerId) a1
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Fleet.Driver.postDriverFleetCashRideUpdate a5 a4 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2 a1
