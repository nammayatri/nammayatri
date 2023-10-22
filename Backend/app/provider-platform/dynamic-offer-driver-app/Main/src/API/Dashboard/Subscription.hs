{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Subscription where

import qualified API.UI.Plan as DPlan
import qualified "dashboard-helper-api" Dashboard.Common as DP
import qualified Domain.Action.UI.Payment as APayment
import qualified Domain.Action.UI.Plan as DTPlan
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Plan as DPlan
import Environment
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import SharedLogic.Merchant
import qualified Storage.Queries.DriverInformation as DI
import Prelude

data SubscriptionEndpoint
  = SelectPlanEndpoint
  | SubscribePlanEndpoint
  | SuspendPlanEndpoint
  deriving (Show, Read)

derivePersistField "SubscriptionEndpoint"

type API =
  "plan"
    :> ( ListPlan
           :<|> SelectPlan
           :<|> SuspendPlan
           :<|> SubscribePlan
           :<|> CurrentPlan
           :<|> OrderStatus
       )

type ListPlan =
  Capture "driverId" (Id DP.Driver)
    :> "list"
    :> Get '[JSON] DTPlan.PlanListAPIRes

type SelectPlan =
  Capture "driverId" (Id DP.Driver)
    :> Capture "planId" (Id DPlan.Plan)
    :> "select"
    :> Put '[JSON] APISuccess

type SuspendPlan =
  Capture "driverId" (Id DP.Driver)
    :> "suspend"
    :> Put '[JSON] APISuccess

type SubscribePlan =
  Capture "driverId" (Id DP.Driver)
    :> Capture "planId" (Id DPlan.Plan)
    :> "subscribe"
    :> Post '[JSON] DTPlan.PlanSubscribeRes

type CurrentPlan =
  Capture "driverId" (Id DP.Driver)
    :> Get '[JSON] DTPlan.CurrentPlanRes

type OrderStatus =
  Capture "driverId" (Id DP.Driver)
    :> Capture "orderId" (Id INV.Invoice)
    :> "status"
    :> Get '[JSON] APayment.PaymentStatusResp

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  planList merchantId
    :<|> planSelect merchantId
    :<|> planSuspend merchantId
    :<|> planSubscribe merchantId
    :<|> currentPlan merchantId
    :<|> paymentStatus merchantId

planList :: ShortId DM.Merchant -> Id DP.Driver -> FlowHandler DTPlan.PlanListAPIRes
planList merchantShortId driverId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  DPlan.planList (cast driverId, m.id) (Just 0) (Just 50)

planSelect :: ShortId DM.Merchant -> Id DP.Driver -> Id DPlan.Plan -> FlowHandler APISuccess
planSelect merchantShortId driverId planId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  DPlan.planSelect planId (cast driverId, m.id)

planSuspend :: ShortId DM.Merchant -> Id DP.Driver -> FlowHandler APISuccess
planSuspend merchantShortId driverId = withFlowHandlerAPI $ do
  m <- findMerchantByShortId merchantShortId
  DTPlan.planSuspend True (cast driverId, m.id)

planSubscribe :: ShortId DM.Merchant -> Id DP.Driver -> Id DPlan.Plan -> FlowHandler DTPlan.PlanSubscribeRes
planSubscribe merchantShortId driverId planId = withFlowHandlerAPI $ do
  m <- findMerchantByShortId merchantShortId
  driverInfo <- DI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  DTPlan.planSubscribe planId True (cast driverId, m.id) driverInfo

currentPlan :: ShortId DM.Merchant -> Id DP.Driver -> FlowHandler DTPlan.CurrentPlanRes
currentPlan merchantShortId driverId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  DPlan.currentPlan (cast driverId, m.id)

paymentStatus :: ShortId DM.Merchant -> Id DP.Driver -> Id INV.Invoice -> FlowHandler APayment.PaymentStatusResp
paymentStatus merchantShortId driverId invoiceId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  withFlowHandlerAPI $ APayment.getStatus (cast driverId, m.id) (cast invoiceId)
