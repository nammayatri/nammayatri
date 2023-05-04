{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.MultipleRideEnd
  ( API,
    handler,
  )
where

import qualified "rider-app" API.Dashboard.MultipleRideEnd as BAP
import qualified "rider-app" Domain.Action.Dashboard.MultipleRideEnd as Domain
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, withFlowHandlerAPI)
import qualified RiderPlatformClient.RiderApp as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth hiding (BECKN_TRANSPORT, DRIVER_OFFER_BPP)
import "lib-dashboard" Tools.Auth.Merchant

type API = MultipleRideEndAPI

type MultipleRideEndAPI =
  ApiAuth 'APP_BACKEND 'RIDES 'MULTIPLERIDE_END -- 'WRITE_ACCESS
    :> BAP.API

handler :: ShortId DM.Merchant -> FlowServer API
handler =
  multipleRideEnd

buildTransactionMultipleRideEnd ::
  ( MonadFlow m,
    Domain.HideSecrets request
  ) =>
  BAP.MultipleRideEndEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransactionMultipleRideEnd endpoint apiTokenInfo =
  T.buildTransaction (DT.MultipleRideEndAPI endpoint) (Just APP_BACKEND) (Just apiTokenInfo) Nothing Nothing

multipleRideEnd :: ShortId DM.Merchant -> ApiTokenInfo -> Domain.MultipleRideEndReq -> FlowHandler APISuccess
multipleRideEnd merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransactionMultipleRideEnd BAP.MultipleRideEndEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId (.multipleRideEnds.multipleRideEnd) req
