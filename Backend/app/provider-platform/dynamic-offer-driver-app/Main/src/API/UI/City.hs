{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.City
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.City as DCity
import qualified Domain.Types.City as DTC
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type API =
  "city"
    :> ( Capture "merchantId" (Id DM.Merchant)
           :> "list"
           :> Get '[JSON] [DTC.CityRes]
       )

handler :: FlowServer API
handler = listCities

listCities :: Id DM.Merchant -> FlowHandler [DTC.CityRes]
listCities = withFlowHandlerAPI . DCity.listCities
