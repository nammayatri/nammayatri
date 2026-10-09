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
module Domain.Action.DashboardAuth.Management.Account
  ( getAccountFetchUnverifiedAccounts,
    postAccountVerifyAccount,
    putAccountUpdateRole,
  )
where

import qualified API.Types.ProviderPlatform.Management.Account
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.Account
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

putAccountUpdateRole :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Person -> Kernel.Types.Id.Id Dashboard.Common.Role -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putAccountUpdateRole _a5 _a4 _a3 a2 a1 = do
  Tools.Auth.DashboardUserAuth.updateDashboardPersonRole a2.getId a1.getId
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success

getAccountFetchUnverifiedAccounts :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Account.FleetOwnerStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow API.Types.ProviderPlatform.Management.Account.UnverifiedAccountsResp)
getAccountFetchUnverifiedAccounts _a9 _a8 _a7 a6 a5 a4 a3 a2 a1 = Tools.Auth.DashboardRegistration.listUnverifiedDashboardAccounts a6 a5 a4 a3 a2 a1

postAccountVerifyAccount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Account.VerifyAccountReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postAccountVerifyAccount a4 a3 a2 a1 = do
  res <- Domain.Action.Dashboard.Management.Account.postAccountVerifyAccount a4 a3 a1
  Tools.Auth.DashboardUserAuth.applyDashboardAccountVerification a2 (Kernel.Types.Id.getId a1.fleetOwnerId) (a1.status Kernel.Prelude.== API.Types.ProviderPlatform.Management.Account.Approved) a1.reason
  Kernel.Prelude.pure res
