{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Provider
  ( API,
    handler,
  )
where

import qualified API.Action.Provider.Fleet as Fleet
import qualified API.Action.Provider.Fleet as Management
import qualified API.Action.Provider.Fleet as Operator
import qualified API.Action.Provider.Fleet as RideBooking
import qualified Domain.Types.Merchant
import Environment
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Id
import Servant hiding (serveDirectoryWebApp)

type API =
  "fleet" :> Fleet.API
    :<|> "management" :> Management.API
    :<|> "operator" :> Operator.API
    :<|> "rideBooking" :> RideBooking.API

handler :: ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> FlowServer API
handler merchantId city =
  Fleet.handler merchantId city
    :<|> Management.handler merchantId city
    :<|> Operator.handler merchantId city
    :<|> RideBooking.handler merchantId city
