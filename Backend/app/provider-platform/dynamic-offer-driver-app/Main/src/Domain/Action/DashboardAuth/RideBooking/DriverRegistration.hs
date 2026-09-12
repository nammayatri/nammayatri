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
module Domain.Action.DashboardAuth.RideBooking.DriverRegistration
  ( postDriverRegistrationVerify,
  )
where

import qualified API.Types.Dashboard.RideBooking.DriverRegistration
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import qualified Domain.Action.Dashboard.RideBooking.DriverRegistration
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

postDriverRegistrationVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthVerifyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationVerify a5 a4 a3 a2 a1 = do
  mbFleet <- Tools.Auth.DashboardUserAuth.requestorFleetFlag a3
  Domain.Action.Dashboard.RideBooking.DriverRegistration.postDriverRegistrationVerify a5 a4 a2 mbFleet (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a1
