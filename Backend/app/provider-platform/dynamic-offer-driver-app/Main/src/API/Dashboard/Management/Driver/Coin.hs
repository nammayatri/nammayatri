{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Management.Driver.Coin where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Coin as Common
import qualified Domain.Action.Dashboard.Driver.Coin as DDriverCoins
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (throwError)

type API =
  "coins"
    :> Common.BulkUploadCoinsAPI
      :<|> Common.CoinHistoryAPI

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantShortId opCity =
  bulkUploadCoins merchantShortId opCity
    :<|> coinHistory merchantShortId opCity

bulkUploadCoins :: ShortId DM.Merchant -> Context.City -> Common.BulkUploadCoinsReq -> FlowHandler APISuccess
bulkUploadCoins merchantShortId opCity = withFlowHandlerAPI . DDriverCoins.bulkUploadCoinsHandler merchantShortId opCity

coinHistory :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Maybe Integer -> Maybe Integer -> FlowHandler Common.CoinHistoryRes
coinHistory merchantShortId opCity driverId mbLimit mbOffset = withFlowHandlerAPI $ DDriverCoins.coinHistoryHandler merchantShortId opCity (cast @Common.Driver @DP.Driver driverId) mbLimit mbOffset
