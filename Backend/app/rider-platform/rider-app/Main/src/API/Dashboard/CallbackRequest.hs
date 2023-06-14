{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.CallbackRequest where

import qualified Domain.Action.Dashboard.CallbackRequest as DC
import qualified Domain.Types.CallbackRequest as DCR
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type API =
  "callback"
    :> ( ListCallbackRequest
           :<|> UpdateCallbackRequestSatus
       )

type ListCallbackRequest =
  "request"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "mobileNumber" Text
    :> QueryParam "createdAt" UTCTime
    :> QueryParam "status" DCR.CallbackRequestStatus
    :> Get '[JSON] DC.CallbackRequestRes

type UpdateCallbackRequestSatus =
  "update" :> "status"
    :> Capture "callbackRequestId" (Id DCR.CallbackRequest)
    :> Capture "status" DCR.CallbackRequestStatus
    :> Post '[JSON] APISuccess

data CallbackRequestEndPoint = CallbackRequestEndPoint
  deriving (Show, Read)

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listCallbackRequest merchantId
    :<|> updateCallbackRequest merchantId

listCallbackRequest :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe DCR.CallbackRequestStatus -> FlowHandler DC.CallbackRequestRes
listCallbackRequest merchantShortId mbLimit mbOffset mbMobileNumber mbCreatedAt mbStatus = withFlowHandlerAPI $ DC.getCallbackRequest merchantShortId mbLimit mbOffset mbMobileNumber mbCreatedAt mbStatus

updateCallbackRequest :: ShortId DM.Merchant -> Id DCR.CallbackRequest -> DCR.CallbackRequestStatus -> FlowHandler APISuccess
updateCallbackRequest merchantShortId id status = withFlowHandlerAPI $ DC.updateCallbackRequestStatus merchantShortId id status
