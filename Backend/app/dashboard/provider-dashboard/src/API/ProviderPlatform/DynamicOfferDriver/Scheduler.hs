{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Scheduler
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Dashboard.Scheduler as DScheduler
import Dashboard.Common as Common
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Scheduler as DScheduler
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, withFlowHandlerAPI)
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth hiding (BECKN_TRANSPORT)
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "scheduler"
    :> ( ListSchedulerAPI
           :<|> UpdateSchedulerAPI
       )

type ListSchedulerAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'SCHEDULER 'LIST_SCHEDULER
    :> DScheduler.ListSchedulerAPI

type UpdateSchedulerAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'SCHEDULER 'UPDATE_SCHEDULER
    :> DScheduler.UpdateSchedulerAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listScheduler merchantId
    :<|> updateScheduler merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  DScheduler.SchedulerEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.SchedulerAPI endpoint) (Just DRIVER_OFFER_BPP) (Just apiTokenInfo) Nothing Nothing

listScheduler :: ShortId DM.Merchant -> ApiTokenInfo -> DScheduler.ListSchedulerReq -> FlowHandler DScheduler.ListSchedulerResp
listScheduler merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.scheduler.listScheduler) req

updateScheduler :: ShortId DM.Merchant -> ApiTokenInfo -> DScheduler.UpdateSchedulerReq -> FlowHandler APISuccess
updateScheduler merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction DScheduler.UpdateSchedulerEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.scheduler.updateScheduler) req
