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
  ( postDriverFleetAddVehicles,
    postDriverFleetAddVehicle,
    postDriverFleetRespondDriverRequest,
    postDriverFleetAddRCWithoutDriver,
    getDriverFleetGetAllVehicle,
    getDriverFleetGetAllDriver,
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
    postDriverFleetDriverUpdate,
    getDriverFleetVehicleListStats,
    getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles,
    getDriverFleetStatusSummary,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2
import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import qualified Data.Time
import qualified Domain.Action.Dashboard.Fleet.Driver
import qualified Domain.Types.Alert.AlertRequestStatus
import qualified Domain.Types.Alert.AlertRequestType
import qualified Domain.Types.FleetBadgeType
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
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
import Tools.Auth
import qualified Tools.Auth.DashboardRegistration
import Tools.Auth.DashboardUserAuth

postDriverFleetAddVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.CreateVehiclesReq -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.APISuccessWithUnprocessedEntities)
postDriverFleetAddVehicles a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a3
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a3) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a3) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddVehicles a5 a4 a1 {API.Types.ProviderPlatform.Fleet.Driver.fleetOwnerId = mbFleetOwnerId, API.Types.ProviderPlatform.Fleet.Driver.requestorId = Kernel.Prelude.Just requestorId}

postDriverFleetAddVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Dashboard.Common.Role) -> API.Types.ProviderPlatform.Fleet.Driver.AddVehicleReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetAddVehicle a8 a7 a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a6
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a6) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a6) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a6) a3
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddVehicle a8 a7 a5 requestorId mbFleetOwnerId a4 a2 a1

postDriverFleetRespondDriverRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.RequestRespondReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetRespondDriverRequest a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetRespondDriverRequest a5 a4 fleetOwnerId a1

postDriverFleetAddRCWithoutDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Dashboard.ProviderPlatform.Management.DriverRegistration.RegisterRCReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetAddRCWithoutDriver a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a3
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddRCWithoutDriver a5 a4 fleetOwnerId a1

getDriverFleetGetAllVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.Driver.ApprovalStatusFilter) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.ListVehicleResT)
getDriverFleetGetAllVehicle a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a11
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetGetAllVehicle a13 a12 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetGetAllDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.Driver.ApprovalStatusFilter) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.Driver.OnboardingAs) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetListDriverResT)
getDriverFleetGetAllDriver a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a15
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetGetAllDriver a17 a16 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

postDriverFleetUnlink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetUnlink a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a4
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a4) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a4) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) a1
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetUnlink a6 a5 requestorId a3 a2 mbFleetOwnerId

postDriverFleetRemoveVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetRemoveVehicle a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a3
  (fleetOwnerId, requestorId) <- SharedLogic.Fleet.getFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a3) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a3) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a1
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetRemoveVehicle a5 a4 fleetOwnerId a2 (Kernel.Prelude.Just requestorId)

postDriverFleetRemoveDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetRemoveDriver a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a4
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a4) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a4) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetRemoveDriver a6 a5 requestorId a3 mbFleetOwnerId a1

getDriverFleetTotalEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetTotalEarningResponse)
getDriverFleetTotalEarning a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetTotalEarning a5 a4 fleetOwnerId a2 a1

getDriverFleetVehicleEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes)
getDriverFleetVehicleEarning a8 a7 a6 a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a6) Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetVehicleEarning a8 a7 fleetOwnerId a5 a4 a3 a2 a1

getDriverFleetDriverEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.SortOn) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes)
getDriverFleetDriverEarning a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a9) Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverEarning a11 a10 fleetOwnerId a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetDriverVehicleAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes)
getDriverFleetDriverVehicleAssociation a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a9
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a9) Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverVehicleAssociation a11 a10 fleetOwnerId a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetDriverListStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.FleetDriverListStatsSortOn) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.FleetDriverStatsResponseType) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetDriverStatsListRes)
getDriverFleetDriverListStats a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a12) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a12) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a12) a9
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverListStats a14 a13 requestorId a11 a10 mbFleetOwnerId a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetDriverAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.DriverMode) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.DocsVerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationResT)
getDriverFleetDriverAssociation a25 a24 a23 a22 a21 a20 a19 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a23
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverAssociation a25 a24 a22 a21 a20 a19 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getDriverFleetVehicleAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.FleetVehicleStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.DocsVerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationResT)
getDriverFleetVehicleAssociation a19 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a17
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetVehicleAssociation a19 a18 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

postDriverFleetVehicleEdit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.EditVehicleReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetVehicleEdit a8 a7 a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a6
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a6) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a6) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a6) a5
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetVehicleEdit a8 a7 requestorId mbFleetOwnerId a4 a3 a2 a1

postDriverFleetVehicleDriverRcStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.RCStatusReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetVehicleDriverRcStatus a6 a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a4
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a4) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a4) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetVehicleDriverRcStatus a6 a5 a3 requestorId mbFleetOwnerId a1

postDriverUpdateFleetOwnerInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.UpdateFleetOwnerInfoReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverUpdateFleetOwnerInfo a6 a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) a2
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
  (fleetOwnerId, requestorId) <- SharedLogic.Fleet.getFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a3) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a3) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetSendJoiningOtp a5 a4 (Tools.Auth.DashboardUserAuth.dashboardRequestorName a3) (Kernel.Prelude.Just fleetOwnerId) (Kernel.Prelude.Just requestorId) a1

postDriverFleetVerifyJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.VerifyFleetJoiningOtpReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetVerifyJoiningOtp a6 a5 a4 a3 a2 a1 = do
  (fleetOwnerId, requestorId) <- SharedLogic.Fleet.getFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a4) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a4) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetVerifyJoiningOtp a6 a5 fleetOwnerId a3 (Kernel.Prelude.Just requestorId) a1

getDriverFleetRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.External.Maps.Types.LatLong) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.RouteAPIResp)
getDriverFleetRoutes a8 a7 a6 a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a6) a3
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetRoutes a8 a7 fleetOwnerId a5 a4 a2 a1

getDriverFleetPossibleRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.RouteAPIResp)
getDriverFleetPossibleRoutes a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetPossibleRoutes a5 a4 fleetOwnerId a1

postDriverFleetTripPlanner :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.TripPlannerReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetTripPlanner a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetTripPlanner a5 a4 fleetOwnerId a1

postDriverFleetAddDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.CreateDriversReq -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.AddDriversResp)
postDriverFleetAddDrivers a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a3
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a3) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a3) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddDrivers a5 a4 (Kernel.Prelude.Just requestorId) a1 {API.Types.ProviderPlatform.Fleet.Driver.fleetOwnerId = mbFleetOwnerId}

postDriverFleetAddDriverBusRouteMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.CreateDriverBusRouteMappingReq -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.APISuccessWithUnprocessedEntities)
postDriverFleetAddDriverBusRouteMapping a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetAddDriverBusRouteMapping a5 a4 a1 {API.Types.ProviderPlatform.Fleet.Driver.fleetOwnerId = Kernel.Prelude.Just fleetOwnerId}

postDriverFleetLinkRCWithDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.LinkRCWithDriverForFleetReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetLinkRCWithDriver a5 a4 a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.checkFleetOwnerVerification a3
  (fleetOwnerId, requestorId) <- SharedLogic.Fleet.getFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a3) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a3) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetLinkRCWithDriver a5 a4 fleetOwnerId (Kernel.Prelude.Just requestorId) a1

postDriverDashboardFleetWmbTripEnd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.TripTransaction -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Dashboard.Common.ActionSource) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverDashboardFleetWmbTripEnd a6 a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverDashboardFleetWmbTripEnd a6 a5 a3 fleetOwnerId a1

getDriverFleetWmbRouteDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.RouteDetails)
getDriverFleetWmbRouteDetails a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a1
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetWmbRouteDetails a5 a4 fleetOwnerId a2

postDriverFleetGetNearbyDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.NearbyDriverReq -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.NearbyDriverRespT)
postDriverFleetGetNearbyDrivers a4 a3 a2 a1 = do
  fleetOwnerIds <- SharedLogic.Fleet.getFleetOwnerIds (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) Kernel.Prelude.Nothing
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
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2
  Domain.Action.Dashboard.Fleet.Driver.postDriverDashboardFleetTrackDriver a5 a4 fleetOwnerId a1

getDriverFleetStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.DriverStatusRes)
getDriverFleetStatus a4 a3 a2 a1 = do
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a2) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a2) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetStatus a4 a3 requestorId mbFleetOwnerId

postDriverFleetGetNearbyDriversV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.NearbyDriversReqV2 -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.NearbyDriversRespTV2)
postDriverFleetGetNearbyDriversV2 a4 a3 a2 a1 = do
  fleetOwnerIds <- SharedLogic.Fleet.getFleetOwnerIds (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) Kernel.Prelude.Nothing
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
  (fleetOwnerId, requestorId) <- SharedLogic.Fleet.getFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a2) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a2) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDashboardAnalyticsAllTime a4 a3 fleetOwnerId (Kernel.Prelude.Just requestorId)

getDriverFleetDashboardAnalytics :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Driver.FleetAnalyticsResponseType) -> Data.Time.Day -> Data.Time.Day -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetAnalyticsRes)
getDriverFleetDashboardAnalytics a7 a6 a5 a4 a3 a2 a1 = do
  (fleetOwnerId, requestorId) <- SharedLogic.Fleet.getFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a5) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a5) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5) a4
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDashboardAnalytics a7 a6 fleetOwnerId (Kernel.Prelude.Just requestorId) a3 a2 a1

postDriverDashboardFleetEstimateRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Fleet.Driver.EstimateRouteReq -> Environment.Flow Kernel.External.Maps.GetRoutesResp)
postDriverDashboardFleetEstimateRoute a5 a4 a3 _a2 a1 = do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.postDriverDashboardFleetEstimateRoute a5 a4 fleetOwnerId a1

postDriverFleetApproveDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.ApproveDriverReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetApproveDriver a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Driver.postDriverFleetApproveDriver a4 a3 fleetOwnerId a1

postDriverFleetDriverUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Fleet.Driver.UpdateDriverReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetDriverUpdate a5 a4 a3 a2 a1 = do
  Kernel.Prelude.whenJust a1.email $ \email -> Tools.Auth.DashboardRegistration.assertDashboardEmailAvailable email a2.getId
  res <- Domain.Action.Dashboard.Fleet.Driver.postDriverFleetDriverUpdate a5 a4 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a1
  Tools.Auth.DashboardRegistration.updateDashboardPersonProfile a2.getId a1.firstName a1.lastName a1.email a1.mobileNo a1.mobileCountryCode
  Kernel.Prelude.pure res

getDriverFleetVehicleListStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Data.Time.Day -> Data.Time.Day -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.FleetVehicleStatsRes)
getDriverFleetVehicleListStats a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  (fleetOwnerId, requestorId) <- SharedLogic.Fleet.getFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a7) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a7) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a7) a6
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetVehicleListStats a9 a8 fleetOwnerId (Kernel.Prelude.Just requestorId) a5 a4 a3 a2 a1

getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.OnboardedDriversAndUnlinkedVehiclesRes)
getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles a6 a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) (Kernel.Prelude.Just a3)
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles a6 a5 fleetOwnerId a2 a1

getDriverFleetStatusSummary :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Driver.EntityOperationType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Driver.StatusSummaryResponse)
getDriverFleetStatusSummary a5 a4 a3 a2 a1 = do
  (mbFleetOwnerId, _) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a3) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a3) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a1
  Domain.Action.Dashboard.Fleet.Driver.getDriverFleetStatusSummary a5 a4 a2 mbFleetOwnerId
