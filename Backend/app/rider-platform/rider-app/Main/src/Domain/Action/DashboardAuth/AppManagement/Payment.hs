{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Hand-written handlers for direct-dashboard payment routes whose request
-- needs more than the verified operator's id.
--
-- rider-dashboard did this work in its own
-- @Domain.Action.RiderPlatform.AppManagement.Payment@ before forwarding the
-- call. The generated @API.Action.DashboardAuth@ handler calls these functions
-- for every endpoint marked @appServerHandler: custom@ in the spec.
module Domain.Action.DashboardAuth.AppManagement.Payment
  ( postPaymentRefundRequestInitiate,
  )
where

import qualified API.Types.Dashboard.AppManagement.Payment
import qualified Domain.Action.Dashboard.AppManagement.Payment
import qualified Domain.Types.Merchant
import qualified Domain.Types.Ride
import qualified "lib-dashboard" Domain.Types.Role as DDashboardRole
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Tools.Auth.DashboardUserAuth

-- | Only a DASHBOARD_ADMIN caller auto-approves the refund they initiate;
-- anyone else's is born awaiting /respond.
--
-- The client sends this flag, but it is discarded: rider-dashboard set it from
-- the caller's role before forwarding, and the application server treats an
-- absent flag as auto-approve, so passing the client's value through would let
-- any operator who may initiate a refund approve it too.
postPaymentRefundRequestInitiate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> API.Types.Dashboard.AppManagement.Payment.RefundRequestInitiateReq -> Environment.Flow API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondResp)
postPaymentRefundRequestInitiate a6 a5 a4 a3 _clientAutoApprove a1 = do
  accessType <- dashboardRequestorAccessType a4
  let autoApprove = accessType == DDashboardRole.DASHBOARD_ADMIN
  Domain.Action.Dashboard.AppManagement.Payment.postPaymentRefundRequestInitiate a6 a5 a3 (Kernel.Prelude.Just autoApprove) (Kernel.Prelude.Just (dashboardRequestorId a4)) a1
