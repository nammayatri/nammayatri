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
import qualified Domain.Action.UI.Plan as DTPlan
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Plan as DPlan
import Environment
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import SharedLogic.Merchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
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

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  planList merchantId city
    :<|> planSelect merchantId city
    :<|> planSuspend merchantId city
    :<|> planSubscribe merchantId city
    :<|> currentPlan merchantId city

planList :: ShortId DM.Merchant -> City.City -> Id DP.Driver -> FlowHandler DTPlan.PlanListAPIRes
planList merchantShortId opCity driverId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  mOCityId <- withFlowHandlerAPI $ CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  DPlan.planList (cast driverId, m.id, mOCityId) (Just 0) (Just 50)

planSelect :: ShortId DM.Merchant -> City.City -> Id DP.Driver -> Id DPlan.Plan -> FlowHandler APISuccess
planSelect merchantShortId opCity driverId planId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  mOCityId <- withFlowHandlerAPI $ CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  DPlan.planSelect planId (cast driverId, m.id, mOCityId)

planSuspend :: ShortId DM.Merchant -> City.City -> Id DP.Driver -> FlowHandler APISuccess
planSuspend merchantShortId opCity driverId = withFlowHandlerAPI $ do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  DTPlan.planSuspend True (cast driverId, m.id, mOCityId)

planSubscribe :: ShortId DM.Merchant -> City.City -> Id DP.Driver -> Id DPlan.Plan -> FlowHandler DTPlan.PlanSubscribeRes
planSubscribe merchantShortId opCity driverId planId = withFlowHandlerAPI $ do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  driverInfo <- DI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  DTPlan.planSubscribe planId True (cast driverId, m.id, mOCityId) driverInfo

currentPlan :: ShortId DM.Merchant -> City.City -> Id DP.Driver -> FlowHandler DTPlan.CurrentPlanRes
currentPlan merchantShortId opCity driverId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  mOCityId <- withFlowHandlerAPI $ CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  DPlan.currentPlan (cast driverId, m.id, mOCityId)
