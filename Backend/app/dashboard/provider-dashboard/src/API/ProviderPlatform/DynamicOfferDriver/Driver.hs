{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module API.ProviderPlatform.DynamicOfferDriver.Driver
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Dashboard.Fleet.Operations as Fleet
import qualified Data.Time as DT
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Ride as DARide
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "dynamic-offer-driver-app" Domain.Types.Person as DP
import qualified "dynamic-offer-driver-app" Domain.Types.Ride as DRide
import "lib-dashboard" Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI')
import qualified ProviderPlatformClient.DynamicOfferDriver.Fleet as Client
import Servant hiding (throwError)
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "driver"
    :> ListDriverRidesForFleetAPI

type ListDriverRidesForFleetAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'LIST_DRIVER_RIDES
    :> Fleet.ListDriverRidesForFleetAPI

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler =
  listDriverRidesForFleet

listDriverRidesForFleet :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id DP.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe DRide.RideStatus -> Maybe DT.Day -> Maybe Text -> FlowHandler DARide.DriverRideListRes
listDriverRidesForFleet merchantShortId opCity apiTokenInfo driverId mbLimit mbOffset mbOnlyActive mbStatus mbDate mbFleetOwnerId =
  withFlowHandlerAPI' $ do
    checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
    Client.callDynamicOfferDriverAppFleetApi checkedMerchantId opCity (.operations.listDriverRidesForFleet) driverId mbLimit mbOffset mbOnlyActive mbStatus mbDate mbFleetOwnerId
