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
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import SharedLogic.Merchant
import Storage.Beam.SystemConfigs ()

data RideEstimatesEndPoint
  = EstimatesEndPoint
  | CancelSearchEndPoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord)

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

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId = callSelect merchantId :<|> callSelectList merchantId :<|> callSelectResult merchantId :<|> callCancelSearch merchantId

callSelect :: ShortId DM.Merchant -> Id DP.Person -> Id DEstimate.Estimate -> FlowHandler APISuccess
callSelect merchantId personId estimateId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantId
  US.select2 (personId, m.id) estimateId $ US.DSelectReq {customerExtraFee = Nothing, autoAssignEnabled = False, autoAssignEnabledV2 = Nothing, paymentMethodId = Nothing}

callSelectList :: ShortId DM.Merchant -> Id DP.Person -> Id DEstimate.Estimate -> FlowHandler DSelect.SelectListRes
callSelectList merchantId personId estimate = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantId
  US.selectList (personId, m.id) estimate

callSelectResult :: ShortId DM.Merchant -> Id DP.Person -> Id DEstimate.Estimate -> FlowHandler DSelect.QuotesResultResponse
callSelectResult merchantId personId estimate = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantId
  US.selectResult (personId, m.id) estimate

callCancelSearch :: ShortId DM.Merchant -> Id DP.Person -> Id DEstimate.Estimate -> FlowHandler DSelect.CancelAPIResponse
callCancelSearch merchantId personId estimate = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantId
  US.cancelSearch (personId, m.id) estimate
