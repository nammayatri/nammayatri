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
module Domain.Action.DashboardAuth.Management.Driver
  ( deleteDriverPermanentlyDelete,
    postDriverTdsRateUpdate,
    getDriverStats,
    getDriverEarnings,
    getDriverFyEarnings,
    getDriverIdentityInfo,
    postDriverIdentityInfoUpdate,
    postDriverAssociationChange,
    postDriverClearFee,
    postDriverRefundByPayout,
  )
where

import qualified API.Types.ProviderPlatform.Management.Driver
import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Data.Time
import qualified Domain.Action.Dashboard.Management.Driver
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.FleetScope
import Tools.Auth
import qualified Tools.Auth.DashboardRegistration
import Tools.Auth.DashboardUserAuth

deleteDriverPermanentlyDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteDriverPermanentlyDelete a4 a3 a2 a1 = do
  SharedLogic.FleetScope.assertDriverInScope a2 (Kernel.Types.Id.cast a1)
  res <- Domain.Action.Dashboard.Management.Driver.deleteDriverPermanentlyDelete a4 a3 a1
  Tools.Auth.DashboardRegistration.deleteDashboardPerson a1.getId
  Kernel.Prelude.pure res

-- | A TDS rate is changed for the fleet owner or driver the request names; for a fleet owner that has to be
-- one their own scope holds.
postDriverTdsRateUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Driver.UpdateTdsRateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverTdsRateUpdate a4 a3 a2 a1 = do
  case a1.ownerType of
    API.Types.ProviderPlatform.Management.Driver.FLEET_OWNER -> SharedLogic.FleetScope.assertFleetInScope a2 (Kernel.Types.Id.getId a1.ownerId)
    API.Types.ProviderPlatform.Management.Driver.DRIVER -> SharedLogic.FleetScope.assertDriverInScope a2 (Kernel.Types.Id.cast a1.ownerId)
  Domain.Action.Dashboard.Management.Driver.postDriverTdsRateUpdate a4 a3 a1

-- The routes below name one driver, or a fleet, by id (or, for the stats, fall back to the caller's own fleet). Once
-- fleet scope is enforced that subject has to be in the caller's scope, and the shared function is told the fleet
-- that holds it is the requestor, so its own "the requestor holds the subject" check holds without the dashboard
-- person being a fleet. Otherwise the dashboard person is the requestor, as before. The fee routes never used the
-- requestor; for an enforced caller the driver has to be in scope.

getDriverStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Data.Time.Day -> Kernel.Prelude.Maybe Data.Time.Day -> Environment.Flow API.Types.ProviderPlatform.Management.Driver.DriverStatsRes)
getDriverStats a6 a5 a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a4 = do
    (entity, requestor) <- case a3 of
      Kernel.Prelude.Just entityId -> (,) a3 <$> SharedLogic.FleetScope.subjectActingFleet a4 entityId.getId
      Kernel.Prelude.Nothing -> (,) Kernel.Prelude.Nothing <$> SharedLogic.FleetScope.scopedFleetOwnerId a4 Kernel.Prelude.Nothing
    Domain.Action.Dashboard.Management.Driver.getDriverStats a6 a5 entity a2 a1 requestor
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Management.Driver.getDriverStats a6 a5 a3 a2 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)

getDriverEarnings :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Data.Time.Day -> Data.Time.Day -> Dashboard.Common.Driver.EarningType -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.Flow API.Types.ProviderPlatform.Management.Driver.EarningPeriodStatsRes)
getDriverEarnings a7 a6 a5 a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a5 = do
    requestor <- SharedLogic.FleetScope.subjectActingFleet a5 a1.getId
    Domain.Action.Dashboard.Management.Driver.getDriverEarnings a7 a6 a4 a3 a2 a1 requestor
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Management.Driver.getDriverEarnings a7 a6 a4 a3 a2 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5)

getDriverFyEarnings :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.Flow API.Types.ProviderPlatform.Management.Driver.FyEarningsRes)
getDriverFyEarnings a6 a5 a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a4 = do
    requestor <- SharedLogic.FleetScope.subjectActingFleet a4 a1.getId
    Domain.Action.Dashboard.Management.Driver.getDriverFyEarnings a6 a5 a3 a2 a1 requestor
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Management.Driver.getDriverFyEarnings a6 a5 a3 a2 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)

getDriverIdentityInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.Flow API.Types.ProviderPlatform.Management.Driver.DriverIdentityInfoRes)
getDriverIdentityInfo a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a2 = do
    requestor <- SharedLogic.FleetScope.subjectActingFleet a2 a1.getId
    Domain.Action.Dashboard.Management.Driver.getDriverIdentityInfo a4 a3 a1 requestor
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Management.Driver.getDriverIdentityInfo a4 a3 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)

postDriverIdentityInfoUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverIdentityInfoReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverIdentityInfoUpdate a5 a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a3 = do
    requestor <- SharedLogic.FleetScope.subjectActingFleet a3 a2.getId
    Domain.Action.Dashboard.Management.Driver.postDriverIdentityInfoUpdate a5 a4 a2 requestor a1
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Management.Driver.postDriverIdentityInfoUpdate a5 a4 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a1

postDriverAssociationChange :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Driver.ChangeAssociationReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverAssociationChange a5 a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a3 = do
    requestor <- SharedLogic.FleetScope.subjectActingFleet a3 a2
    Domain.Action.Dashboard.Management.Driver.postDriverAssociationChange a5 a4 requestor a2 a1
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Management.Driver.postDriverAssociationChange a5 a4 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a2 a1

postDriverClearFee :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.ClearDriverFeeReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverClearFee a5 a4 a3 a2 a1 = do
  SharedLogic.FleetScope.assertDriverInScope a3 (Kernel.Types.Id.cast a2)
  Domain.Action.Dashboard.Management.Driver.postDriverClearFee a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1

postDriverRefundByPayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.RefundByPayoutReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverRefundByPayout a5 a4 a3 a2 a1 = do
  SharedLogic.FleetScope.assertDriverInScope a3 (Kernel.Types.Id.cast a2)
  Domain.Action.Dashboard.Management.Driver.postDriverRefundByPayout a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
