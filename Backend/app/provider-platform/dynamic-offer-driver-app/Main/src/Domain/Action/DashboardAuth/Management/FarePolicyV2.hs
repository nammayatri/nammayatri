{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Hand-written handlers for the direct-dashboard fare-policy routes that
-- carry the acting operator in their request body.
--
-- provider-dashboard stamped those fields from the session before forwarding
-- (@Domain.Action.ProviderPlatform.Management.FarePolicyV2@). The maker-checker
-- rule is enforced on the application server by comparing them, so they must
-- not come from the client.
module Domain.Action.DashboardAuth.Management.FarePolicyV2
  ( postFarePolicyV2ProductRemove,
    postFarePolicyV2ChangeRequestDecide,
  )
where

import qualified API.Types.ProviderPlatform.Management.FarePolicyV2
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.FarePolicyV2
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Tools.Auth.DashboardUserAuth

-- | Who filed the removal request. The checker must differ from the requester,
-- so a client-supplied value would let one operator file as another.
postFarePolicyV2ProductRemove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.FareProduct -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2RemoveProductReq -> Environment.Flow API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ChangeRequestRes)
postFarePolicyV2ProductRemove a5 a4 a3 a2 a1 =
  Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2ProductRemove a5 a4 a2 $
    a1 {API.Types.ProviderPlatform.Management.FarePolicyV2.requestedBy = Kernel.Prelude.Just (dashboardRequestorId a3)}

-- | Who approved or rejected it. The application server rejects a decision
-- whose checkedBy equals the request's requestedBy, which only holds when both
-- come from the session.
postFarePolicyV2ChangeRequestDecide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.FarePolicyChangeRequest -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2DecideChangeRequestReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFarePolicyV2ChangeRequestDecide a5 a4 a3 a2 a1 =
  Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2ChangeRequestDecide a5 a4 a2 $
    a1 {API.Types.ProviderPlatform.Management.FarePolicyV2.checkedBy = Kernel.Prelude.Just (dashboardRequestorId a3)}
