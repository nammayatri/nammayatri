{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Dashboard.RideBooking.Select where

import qualified API.UI.Select as US
import qualified Domain.Action.UI.Select as DSelect
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Servant

data RideEstimatesEndPoint
  = EstimatesEndPoint
  | CancelSearchEndPoint
  deriving (Show, Read)

derivePersistField "RideEstimatesEndPoint"

type API =
  "select"
    :> ( CustomerSelectAPI
           :<|> CustomerSelectListAPI
           :<|> CustomerSelectResultAPI
           :<|> CustomerCancelSearchAPI
       )

type CustomerSelectAPI =
  "estimate"
    :> Capture "customerId" (Id DP.Person)
    :> Capture "estimateId" (Id DEstimate.Estimate)
    :> "select"
    :> Post '[JSON] APISuccess

type CustomerSelectListAPI =
  Capture "customerId" (Id DP.Person)
    :> Capture "estimateId" (Id DEstimate.Estimate)
    :> "quotes"
    :> Get '[JSON] DSelect.SelectListRes

type CustomerSelectResultAPI =
  Capture "customerId" (Id DP.Person)
    :> Capture "estimateId" (Id DEstimate.Estimate)
    :> "result"
    :> Get '[JSON] DSelect.QuotesResultResponse

type CustomerCancelSearchAPI =
  Capture "customerId" (Id DP.Person)
    :> Capture "estimateId" (Id DEstimate.Estimate)
    :> "cancel"
    :> Post '[JSON] DSelect.CancelAPIResponse

handler :: FlowServer API
handler = callSelect :<|> callSelectList :<|> callSelectResult :<|> callCancelSearch

callSelect :: Id DP.Person -> Id DEstimate.Estimate -> FlowHandler APISuccess
callSelect = US.select

callSelectList :: Id DP.Person -> Id DEstimate.Estimate -> FlowHandler DSelect.SelectListRes
callSelectList = US.selectList

callSelectResult :: Id DP.Person -> Id DEstimate.Estimate -> FlowHandler DSelect.QuotesResultResponse
callSelectResult = US.selectResult

callCancelSearch :: Id DP.Person -> Id DEstimate.Estimate -> FlowHandler DSelect.CancelAPIResponse
callCancelSearch = US.cancelSearch
