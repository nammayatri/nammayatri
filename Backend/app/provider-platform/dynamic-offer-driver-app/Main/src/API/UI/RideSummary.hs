{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.RideSummary where

import Data.Time (Day)
import Domain.Action.UI.RideSummary as RDS
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "rideSummary"
    :> ( "list"
           :> TokenAuth
           :> ReqBody '[JSON] [Day]
           :> Post '[JSON] RDS.DriverRideSummaryListResp
       )

handler :: FlowServer API
handler = listDailyStats

listDailyStats :: (Id DP.Person, Id Merchant.Merchant, Id DM.MerchantOperatingCity) -> [Day] -> FlowHandler RDS.DriverRideSummaryListResp
listDailyStats (driverId, merchantId, merchantOpCityId) = withFlowHandlerAPI . RDS.listDailyRidesSummary driverId merchantId merchantOpCityId
