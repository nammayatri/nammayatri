{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Management.Driver.GoHome where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (throwError)

type API =
  "driver"
    :> ( Common.GetDriverHomeLocationAPI
           :<|> Common.UpdateDriverHomeLocationAPI
           :<|> Common.IncrementDriverGoToCountAPI
           :<|> Common.GetDriverGoHomeInfoAPI
       )

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantId city =
  getDriverHomeLocation merchantId city
    :<|> updateDriverHomeLocation merchantId city
    :<|> incrementDriverGoToCount merchantId city
    :<|> getDriverGoHomeInfo merchantId city

getDriverHomeLocation :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler Common.GetHomeLocationsRes
getDriverHomeLocation merchantShortId opCity = withFlowHandlerAPI . DDriver.getDriverHomeLocation merchantShortId opCity

updateDriverHomeLocation :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateDriverHomeLocationReq -> FlowHandler APISuccess
updateDriverHomeLocation merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.updateDriverHomeLocation merchantShortId opCity driverId

incrementDriverGoToCount :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
incrementDriverGoToCount merchantShortId opCity = withFlowHandlerAPI . DDriver.incrementDriverGoToCount merchantShortId opCity

getDriverGoHomeInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler Common.CachedGoHomeRequestInfoRes
getDriverGoHomeInfo merchantShortId opCity = withFlowHandlerAPI . DDriver.getDriverGoHomeInfo merchantShortId opCity
