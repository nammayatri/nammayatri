{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Fleet.Operations where

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
    :> ( AddVehicleForFleetAPI
           :<|> GetAllVehicleForFleetAPI
           :<|> GetAllDriverForFleetAPI
           :<|> FleetUnlinkVehicleAPI
           :<|> FleetRemoveVehicleAPI
           :<|> FleetRemoveDriverAPI
           :<|> FleetTotalEarningAPI
           :<|> FleetVehicleEarningAPI
           :<|> FleetDriverEarningAPI
           :<|> GetFleetDriverVehicleAssociationAPI
           :<|> GetFleetDriverAssociationAPI
           :<|> GetFleetVehicleAssociationAPI
           :<|> SetVehicleDriverRcStatusForFleetAPI
       )

-----------------------------------

-- --- add vehicle for driver api so here  we are passing the fleet owner api----

type AddVehicleForFleetAPI =
  Capture "mobileNo" Text
    :> QueryParam "mobileCountryCode" Text
    :> Capture "fleetOwnerId" Text
    :> "fleet"
    :> "addVehicle"
    :> ReqBody '[JSON] Common.AddVehicleReq
    :> Post '[JSON] APISuccess

-- --- add vehicle for driver api so here  we are passing the fleet owner api----

type GetAllVehicleForFleetAPI =
  Capture "fleetOwnerId" Text
    :> "fleet"
    :> "getAllVehicle"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] Common.ListVehicleRes

type GetAllDriverForFleetAPI =
  Capture "fleetOwnerId" Text
    :> "fleet"
    :> "getAllDriver"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] Common.FleetListDriverRes

type FleetUnlinkVehicleAPI =
  Capture "fleetOwnerId" Text
    :> Capture "driverId" (Id Common.Driver)
    :> Capture "vehicleNo" Text
    :> "fleet"
    :> "unlink"
    :> Post '[JSON] APISuccess

type FleetRemoveVehicleAPI =
  Capture "fleetOwnerId" Text
    :> Capture "vehicleNo" Text
    :> "fleet"
    :> "remove"
    :> "vehicle"
    :> Post '[JSON] APISuccess

type FleetRemoveDriverAPI =
  Capture "fleetOwnerId" Text
    :> Capture "driverId" (Id Common.Driver)
    :> "fleet"
    :> "remove"
    :> "driver"
    :> Post '[JSON] APISuccess

type SetVehicleDriverRcStatusForFleetAPI =
  Capture "driverId" (Id Common.Driver)
    :> Capture "fleetOwnerId" Text
    :> "fleet"
    :> "vehicleDriverRCstatus"
    :> ReqBody '[JSON] Common.RCStatusReq
    :> Post '[JSON] APISuccess

type FleetTotalEarningAPI =
  Capture "fleetOwnerId" Text
    :> "fleet"
    :> "totalEarning"
    :> Get '[JSON] Common.FleetTotalEarningResponse

type FleetVehicleEarningAPI =
  Capture "fleetOwnerId" Text
    :> "fleet"
    :> "vehicleEarning"
    :> Capture "vehicleNo" Text
    :> QueryParam "driverId" (Id Common.Driver)
    :> Get '[JSON] Common.FleetEarningRes

type FleetDriverEarningAPI =
  Capture "fleetOwnerId" Text
    :> Capture "driverId" (Id Common.Driver)
    :> "fleet"
    :> "driverEarning"
    :> Get '[JSON] Common.FleetEarningRes

type GetFleetDriverVehicleAssociationAPI =
  Capture "fleetOwnerId" Text
    :> "fleet"
    :> "getFleetDriverVehicleAssociation"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] Common.DrivertoVehicleAssociationRes

type GetFleetDriverAssociationAPI =
  Capture "fleetOwnerId" Text
    :> "fleet"
    :> "getFleetDriverAssociation"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] Common.DrivertoVehicleAssociationRes

type GetFleetVehicleAssociationAPI =
  Capture "fleetOwnerId" Text
    :> "fleet"
    :> "getFleetVehicleAssociation"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] Common.DrivertoVehicleAssociationRes

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantId city =
  addVehicleForFleet merchantId city
    :<|> getAllVehicleForFleet merchantId city
    :<|> getAllDriverForFleet merchantId city
    :<|> fleetUnlinkVehicle merchantId city
    :<|> fleetRemoveVehicle merchantId city
    :<|> fleetRemoveDriver merchantId city
    :<|> fleetTotalEarning merchantId city
    :<|> fleetVehicleEarning merchantId city
    :<|> fleetDriverEarning merchantId city
    :<|> getFleetDriverVehicleAssociation merchantId city
    :<|> getFleetDriverAssociation merchantId city
    :<|> getFleetVehicleAssociation merchantId city
    :<|> setVehicleDriverRcStatusForFleet merchantId city

addVehicleForFleet :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Text -> Text -> Common.AddVehicleReq -> FlowHandler APISuccess
addVehicleForFleet merchantShortId opCity phoneNo mbMobileCountryCode fleetOwnerId = withFlowHandlerAPI . DDriver.addVehicleForFleet merchantShortId opCity phoneNo mbMobileCountryCode fleetOwnerId

getAllVehicleForFleet :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> FlowHandler Common.ListVehicleRes
getAllVehicleForFleet merchantShortId opCity fleetOwnerId mbLimit mbOffset = withFlowHandlerAPI $ DDriver.getAllVehicleForFleet merchantShortId opCity fleetOwnerId mbLimit mbOffset

getAllDriverForFleet :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> FlowHandler Common.FleetListDriverRes
getAllDriverForFleet merchantShortId _opCity fleetOwnerId mbLimit mbOffset = withFlowHandlerAPI $ DDriver.getAllDriverForFleet merchantShortId fleetOwnerId mbLimit mbOffset

fleetUnlinkVehicle :: ShortId DM.Merchant -> Context.City -> Text -> Id Common.Driver -> Text -> FlowHandler APISuccess
fleetUnlinkVehicle merchantShortId _opCity fleetOwnerId vehicleNo = withFlowHandlerAPI . DDriver.fleetUnlinkVehicle merchantShortId fleetOwnerId vehicleNo

fleetRemoveVehicle :: ShortId DM.Merchant -> Context.City -> Text -> Text -> FlowHandler APISuccess
fleetRemoveVehicle merchantShortId opCity fleetOwnerId = withFlowHandlerAPI . DDriver.fleetRemoveVehicle merchantShortId opCity fleetOwnerId

fleetRemoveDriver :: ShortId DM.Merchant -> Context.City -> Text -> Id Common.Driver -> FlowHandler APISuccess
fleetRemoveDriver merchantShortId opCity fleetOwnerId = withFlowHandlerAPI . DDriver.fleetRemoveDriver merchantShortId opCity fleetOwnerId

fleetTotalEarning :: ShortId DM.Merchant -> Context.City -> Text -> FlowHandler Common.FleetTotalEarningResponse
fleetTotalEarning merchantShortId opCity = withFlowHandlerAPI . DDriver.fleetTotalEarning merchantShortId opCity

fleetVehicleEarning :: ShortId DM.Merchant -> Context.City -> Text -> Text -> Maybe (Id Common.Driver) -> FlowHandler Common.FleetEarningRes
fleetVehicleEarning merchantShortId opCity fleetOwnerId vehicleNo mbDriverId = withFlowHandlerAPI $ DDriver.fleetVehicleEarning merchantShortId opCity fleetOwnerId vehicleNo mbDriverId

fleetDriverEarning :: ShortId DM.Merchant -> Context.City -> Text -> Id Common.Driver -> FlowHandler Common.FleetEarningRes
fleetDriverEarning merchantShortId opCity fleetOwnerId driverId = withFlowHandlerAPI $ DDriver.fleetDriverEarning merchantShortId opCity fleetOwnerId driverId

setVehicleDriverRcStatusForFleet :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Common.RCStatusReq -> FlowHandler APISuccess
setVehicleDriverRcStatusForFleet merchantShortId opCity driverId fleetOwnerId req = withFlowHandlerAPI $ DDriver.setVehicleDriverRcStatusForFleet merchantShortId opCity driverId fleetOwnerId req

getFleetDriverVehicleAssociation :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> FlowHandler Common.DrivertoVehicleAssociationRes
getFleetDriverVehicleAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset = withFlowHandlerAPI $ DDriver.getFleetDriverVehicleAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset

getFleetDriverAssociation :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> FlowHandler Common.DrivertoVehicleAssociationRes
getFleetDriverAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset = withFlowHandlerAPI $ DDriver.getFleetDriverAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset

getFleetVehicleAssociation :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> FlowHandler Common.DrivertoVehicleAssociationRes
getFleetVehicleAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset = withFlowHandlerAPI $ DDriver.getFleetVehicleAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset
