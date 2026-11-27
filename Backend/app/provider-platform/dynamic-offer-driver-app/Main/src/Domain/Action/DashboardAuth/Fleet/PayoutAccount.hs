{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handlers for the fleet payout-account routes of direct-dashboard. The account is a fleet's: the one
-- the request names or the only one the caller has. Once fleet scope is enforced that fleet comes from the caller's
-- scope and is passed on as the requestor too, so the shared function's own-fleet rule holds and the fleet member
-- association is not consulted. Otherwise the dashboard person is the requestor, as before.
module Domain.Action.DashboardAuth.Fleet.PayoutAccount
  ( postPayoutAccount,
    postPayoutAccountStatus,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.PayoutAccount
import qualified Domain.Action.Dashboard.Fleet.PayoutAccount
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.FleetScope
import Tools.Auth
import Tools.Auth.DashboardUserAuth

postPayoutAccount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountReq -> Environment.Flow API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountResp)
postPayoutAccount a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a2 = do
    (_, requestorId) <- SharedLogic.FleetScope.scopedFleetOwnerAndRequestorId a2 (a1 :: API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountReq).fleetOwnerId
    Domain.Action.Dashboard.Fleet.PayoutAccount.postPayoutAccount a4 a3 requestorId a1
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Fleet.PayoutAccount.postPayoutAccount a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

postPayoutAccountStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountStatusReq -> Environment.Flow API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountStatusResp)
postPayoutAccountStatus a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a2 = do
    (_, requestorId) <- SharedLogic.FleetScope.scopedFleetOwnerAndRequestorId a2 (a1 :: API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountStatusReq).fleetOwnerId
    Domain.Action.Dashboard.Fleet.PayoutAccount.postPayoutAccountStatus a4 a3 requestorId a1
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Fleet.PayoutAccount.postPayoutAccountStatus a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1
