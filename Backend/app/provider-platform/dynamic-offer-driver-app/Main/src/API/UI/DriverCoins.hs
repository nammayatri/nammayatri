{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.DriverCoins where

import qualified Domain.Action.UI.DriverCoin as Domain
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment (FlowHandler, FlowServer)
import EulerHS.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "coins"
    :> ( "transactions"
           :> TokenAuth
           :> MandatoryQueryParam "date" UTCTime
           :> Get '[JSON] Domain.CoinTransactionRes
           :<|> "usageHistory"
             :> TokenAuth
             :> Get '[JSON] Domain.CoinsUsageRes
           :<|> "convertCoinToCash"
             :> TokenAuth
             :> ReqBody '[JSON] Domain.ConvertCoinToCashReq
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  getCoinEventSummary
    :<|> getCoinUsageSummary
    :<|> useCoinsHandler

getCoinEventSummary :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> UTCTime -> FlowHandler Domain.CoinTransactionRes
getCoinEventSummary (personId, merchantId, merchantOpCityId) = withFlowHandlerAPI . Domain.getCoinEventSummary (personId, merchantId, merchantOpCityId)

getCoinUsageSummary :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler Domain.CoinsUsageRes
getCoinUsageSummary = withFlowHandlerAPI . Domain.getCoinUsageSummary

useCoinsHandler :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Domain.ConvertCoinToCashReq -> FlowHandler APISuccess
useCoinsHandler (personId, merchantId, merchantOpCityId) = withFlowHandlerAPI . Domain.useCoinsHandler (personId, merchantId, merchantOpCityId)
