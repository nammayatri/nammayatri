{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Fleet.Operations where

import Data.Time
import qualified Domain.Action.UI.Ride as DARide
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()

type API =
  "driver"
    :> ListDriverRidesForFleetAPI

type ListDriverRidesForFleetAPI =
  "fleet"
    :> "listRides"
    :> Capture "driverId" (Id DP.Person)
    :> QueryParam "limit" Integer
    :> QueryParam "offset" Integer
    :> QueryParam "onlyActive" Bool
    :> QueryParam "status" DRide.RideStatus
    :> QueryParam "day" Day
    :> QueryParam "fleetOwnerId" Text
    :> QueryParam "numOfDays" Int
    :> Get '[JSON] DARide.DriverRideListRes

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler = listDriverRidesForFleet

listDriverRidesForFleet :: ShortId DM.Merchant -> Context.City -> Id DP.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe DRide.RideStatus -> Maybe Day -> Maybe Text -> Maybe Int -> FlowHandler DARide.DriverRideListRes
listDriverRidesForFleet _ _ driverId mbLimit mbOffset mbOnlyActive mbStatus mbDay mbFleetOwnerId mbNumOfDays = withFlowHandlerAPI $ DARide.listDriverRides driverId mbLimit mbOffset mbOnlyActive mbStatus mbDay mbFleetOwnerId mbNumOfDays
