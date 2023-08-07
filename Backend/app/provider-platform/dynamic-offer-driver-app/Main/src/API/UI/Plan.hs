{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Plan where

import qualified Domain.Action.UI.Plan as DPlan
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Plan as DPlan
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "plan"
    :> ( "list"
           :> TokenAuth
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> QueryParam "paymentType" DPlan.PaymentMode
           :> Get '[JSON] DPlan.PlanListAPIRes
           :<|> "pause"
             :> TokenAuth
             :> Put '[JSON] APISuccess
           :<|> "resume"
             :> TokenAuth
             :> Put '[JSON] APISuccess
           :<|> "upgrade"
             :> TokenAuth
             :> Put '[JSON] DPlan.PlanUpgradeRes
           :<|> "downgrade"
             :> TokenAuth
             :> Put '[JSON] APISuccess
           :<|> Capture "planId" (Id DPlan.Plan)
             :> "subscribe"
             :> TokenAuth
             :> ReqBody '[JSON] DPlan.PlanSubscribeReq
             :> Post '[JSON] DPlan.PlanSubscribeRes
           :<|> Capture "planId" (Id DPlan.Plan)
             :> "select"
             :> TokenAuth
             :> Put '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  planList
    :<|> planPause
    :<|> planResume
    :<|> planUpgradeToAutopay
    :<|> planDowngradeToManual
    :<|> planSubscribe
    :<|> planSelect

planList :: (Id SP.Person, Id DM.Merchant) -> Maybe Int -> Maybe Int -> Maybe DPlan.PaymentMode -> FlowHandler DPlan.PlanListAPIRes
planList (driverId, merchantId) mbLimit mbOffset = withFlowHandlerAPI . DPlan.planList (driverId, merchantId) mbLimit mbOffset

planPause :: (Id SP.Person, Id DM.Merchant) -> FlowHandler APISuccess
planPause = withFlowHandlerAPI . DPlan.planPause

planResume :: (Id SP.Person, Id DM.Merchant) -> FlowHandler APISuccess
planResume = withFlowHandlerAPI . DPlan.planResume

planUpgradeToAutopay :: (Id SP.Person, Id DM.Merchant) -> FlowHandler DPlan.PlanUpgradeRes
planUpgradeToAutopay = withFlowHandlerAPI . DPlan.planUpgradeToAutopay

planDowngradeToManual :: (Id SP.Person, Id DM.Merchant) -> FlowHandler APISuccess
planDowngradeToManual = withFlowHandlerAPI . DPlan.planDowngradeToManual

planSubscribe :: Id DPlan.Plan -> (Id SP.Person, Id DM.Merchant) -> DPlan.PlanSubscribeReq -> FlowHandler DPlan.PlanSubscribeRes
planSubscribe planId (driverId, merchantId) = withFlowHandlerAPI . DPlan.planSubscribe planId (driverId, merchantId)

planSelect :: Id DPlan.Plan -> (Id SP.Person, Id DM.Merchant) -> FlowHandler APISuccess
planSelect planId = withFlowHandlerAPI . DPlan.planSelect planId
