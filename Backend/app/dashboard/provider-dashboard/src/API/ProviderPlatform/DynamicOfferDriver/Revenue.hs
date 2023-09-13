{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Revenue
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Revenue as Common
import Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import Domain.Types.ServerName
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import Servant
import Tools.Auth.Api
import Tools.Auth.Merchant

type API =
  "revenue"
    :> ( CashCollectionHistoryAPI
           :<|> AllDriverFeeHistoryAPI
       )

type CashCollectionHistoryAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'VOLUNTEER 'VOLUNTEER_CASH_COLLECTION_HISTORY
    :> Common.GetCashCollectionHistory

type AllDriverFeeHistoryAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'VOLUNTEER 'ALL_FEE_HISTORY -- change
    :> Common.GetAllDriverFeeHistory

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  getCashCollectionHistory merchantId
    :<|> getAllDriverFeeHistory merchantId

getCashCollectionHistory :: ShortId DM.Merchant -> ApiTokenInfo -> Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Int -> Maybe Int -> FlowHandler Common.CashCollectionListRes
getCashCollectionHistory merchantShortId apiTokenInfo collectorId mbFrom mbTo mbLimit mbOffset = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.revenue.getCashCollectionHistory) collectorId mbFrom mbTo mbLimit mbOffset

getAllDriverFeeHistory :: ShortId DM.Merchant -> ApiTokenInfo -> Maybe UTCTime -> Maybe UTCTime -> FlowHandler Common.AllDriverFeeRes
getAllDriverFeeHistory merchantShortId apiTokenInfo mbFrom mbTo = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.revenue.getAllDriverFeeHistory) mbFrom mbTo
