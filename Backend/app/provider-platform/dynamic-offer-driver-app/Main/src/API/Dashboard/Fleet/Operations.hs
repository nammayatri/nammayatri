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
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Registration as Common
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()

type API =
  "driver"
    :> ( AddVehicleForFleetAPI
           :<|> RegisterRCForFleetWithoutDriverAPI
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
           :<|> Common.UpdateFleetOwnerInfoAPI
           :<|> Common.GetFleetOwnerInfoAPI
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

------ Register RC without Driver so  here  we are passing the fleet owner Id as DriverId  api----

type RegisterRCForFleetWithoutDriverAPI =
  Capture "fleetOwnerId" Text
    :> "fleet"
    :> "addRC"
    :> "withoutDriver"
    :> ReqBody '[JSON] Common.RegisterRCReq
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
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> Get '[JSON] Common.FleetTotalEarningResponse

type FleetVehicleEarningAPI =
  Capture "fleetOwnerId" Text
    :> "fleet"
    :> "vehicleEarning"
    :> QueryParam "vehicleNo" Text
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> Get '[JSON] Common.FleetEarningListRes

type FleetDriverEarningAPI =
  Capture "fleetOwnerId" Text
    :> "fleet"
    :> "driverEarning"
    :> QueryParam "mobileCountryCode" Text
    :> QueryParam "mobileNo" Text
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> Get '[JSON] Common.FleetEarningListRes

type GetFleetDriverVehicleAssociationAPI =
  Capture "fleetOwnerId" Text
    :> "fleet"
    :> "getFleetDriverVehicleAssociation"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "countryCode" Text
    :> QueryParam "phoneNo" Text
    :> QueryParam "vehicleNo" Text
    :> QueryParam "includeStats" Bool
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> Get '[JSON] Common.DrivertoVehicleAssociationRes

type GetFleetDriverAssociationAPI =
  Capture "fleetOwnerId" Text
    :> "fleet"
    :> "getFleetDriverAssociation"
    :> QueryParam "isActive" Bool
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "countryCode" Text
    :> QueryParam "phoneNo" Text
    :> QueryParam "includeStats" Bool
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> QueryParam "status" Common.DriverMode
    :> Get '[JSON] Common.DrivertoVehicleAssociationRes

type GetFleetVehicleAssociationAPI =
  Capture "fleetOwnerId" Text
    :> "fleet"
    :> "getFleetVehicleAssociation"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "vehicleNo" Text
    :> QueryParam "includeStats" Bool
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> QueryParam "status" Common.FleetVehicleStatus
    :> Get '[JSON] Common.DrivertoVehicleAssociationRes

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantId city =
  addVehicleForFleet merchantId city
    :<|> registerRCForFleetWithoutDriver merchantId city
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
    :<|> updateFleetOwnerInfo merchantId city
    :<|> getFleetOwnerInfo merchantId city

addVehicleForFleet :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Text -> Text -> Common.AddVehicleReq -> FlowHandler APISuccess
addVehicleForFleet merchantShortId opCity phoneNo mbMobileCountryCode fleetOwnerId = withFlowHandlerAPI . DDriver.addVehicleForFleet merchantShortId opCity phoneNo mbMobileCountryCode fleetOwnerId

registerRCForFleetWithoutDriver :: ShortId DM.Merchant -> Context.City -> Text -> Common.RegisterRCReq -> FlowHandler APISuccess
registerRCForFleetWithoutDriver merchantShortId opCity fleetOwnerId = withFlowHandlerAPI . DDriver.registerRCForFleetWithoutDriver merchantShortId opCity fleetOwnerId

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

fleetTotalEarning :: ShortId DM.Merchant -> Context.City -> Text -> Maybe UTCTime -> Maybe UTCTime -> FlowHandler Common.FleetTotalEarningResponse
fleetTotalEarning merchantShortId opCity fleetOwnerId mbFrom mbTo = withFlowHandlerAPI $ DDriver.fleetTotalEarning merchantShortId opCity fleetOwnerId mbFrom mbTo

fleetVehicleEarning :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> FlowHandler Common.FleetEarningListRes
fleetVehicleEarning merchantShortId opCity fleetOwnerId vehicleNo mbLimit mbOffset mbFrom mbTo = withFlowHandlerAPI $ DDriver.fleetVehicleEarning merchantShortId opCity fleetOwnerId vehicleNo mbLimit mbOffset mbFrom mbTo

fleetDriverEarning :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> FlowHandler Common.FleetEarningListRes
fleetDriverEarning merchantShortId opCity fleetOwnerId mbMobileCountryCode mbMobileNo mbLimit mbOffset mbFrom mbTo = withFlowHandlerAPI $ DDriver.fleetDriverEarning merchantShortId opCity fleetOwnerId mbMobileCountryCode mbMobileNo mbLimit mbOffset mbFrom mbTo

setVehicleDriverRcStatusForFleet :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Common.RCStatusReq -> FlowHandler APISuccess
setVehicleDriverRcStatusForFleet merchantShortId opCity driverId fleetOwnerId req = withFlowHandlerAPI $ DDriver.setVehicleDriverRcStatusForFleet merchantShortId opCity driverId fleetOwnerId req

getFleetDriverVehicleAssociation :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> FlowHandler Common.DrivertoVehicleAssociationRes
getFleetDriverVehicleAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset mbCountryCode mbPhoneNo mbVehicleNo mbStatus mbFrom mbTo = withFlowHandlerAPI $ DDriver.getFleetDriverVehicleAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset mbCountryCode mbPhoneNo mbVehicleNo mbStatus mbFrom mbTo

getFleetDriverAssociation :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Common.DriverMode -> FlowHandler Common.DrivertoVehicleAssociationRes
getFleetDriverAssociation merchantShortId opCity fleetOwnerId mbIsActive mbLimit mbOffset mbCountryCode mbPhoneNo mbStats mbFrom mbTo mbMode = withFlowHandlerAPI $ DDriver.getFleetDriverAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset mbCountryCode mbPhoneNo mbIsActive mbStats mbFrom mbTo mbMode

getFleetVehicleAssociation :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Common.FleetVehicleStatus -> FlowHandler Common.DrivertoVehicleAssociationRes
getFleetVehicleAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset mbVehicleNo mbIncludeStats mbFrom mbTo mbStatus = withFlowHandlerAPI $ DDriver.getFleetVehicleAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset mbVehicleNo mbIncludeStats mbFrom mbTo mbStatus

updateFleetOwnerInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateFleetOwnerInfoReq -> FlowHandler APISuccess
updateFleetOwnerInfo merchantShortId opCity driverId req = withFlowHandlerAPI $ DDriver.updateFleetOwnerInfo merchantShortId opCity driverId req

getFleetOwnerInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler Common.FleetOwnerInfoRes
getFleetOwnerInfo merchantShortId opCity driverId = withFlowHandlerAPI $ DDriver.getFleetOwnerInfo merchantShortId opCity driverId
