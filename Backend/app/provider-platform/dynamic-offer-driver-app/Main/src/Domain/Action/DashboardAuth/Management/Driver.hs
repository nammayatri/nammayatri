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
import Tools.Auth
import qualified Tools.Auth.DashboardRegistration
import Tools.Auth.DashboardUserAuth

deleteDriverPermanentlyDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteDriverPermanentlyDelete a4 a3 _a2 a1 = do
  res <- Domain.Action.Dashboard.Management.Driver.deleteDriverPermanentlyDelete a4 a3 a1
  Tools.Auth.DashboardRegistration.deleteDashboardPerson a1.getId
  Kernel.Prelude.pure res
