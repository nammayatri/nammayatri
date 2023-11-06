{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.RideBooking.Driver where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Registration as Common
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Action.Dashboard.Driver.Registration as DReg
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type AuthAPI =
  Common.AuthAPI
    :<|> VerifyAPI

type VerifyAPI =
  Capture "authId" Text
    :> Capture "mbFleet" Bool
    :> Capture "fleetOwnerId" Text
    :> "verify"
    :> ReqBody '[JSON] Common.AuthVerifyReq
    :> Post '[JSON] APISuccess

authHandler :: ShortId DM.Merchant -> Context.City -> FlowServer AuthAPI
authHandler merchantId city =
  auth merchantId city
    :<|> verify

auth :: ShortId DM.Merchant -> Context.City -> Common.AuthReq -> FlowHandler Common.AuthRes
auth merchantShortId opCity = withFlowHandlerAPI . DReg.auth merchantShortId opCity

verify :: Text -> Bool -> Text -> Common.AuthVerifyReq -> FlowHandler APISuccess
verify authId mbFleet fleetOwnerId req = withFlowHandlerAPI $ DReg.verify authId mbFleet fleetOwnerId req

type ActivateAPI =
  "driver"
    :> ( Common.DriverOutstandingBalanceAPI
           :<|> Common.EnableDriverAPI
           :<|> DriverCashCollectionAPI
           :<|> DriverCashExemptionAPI
           :<|> DriverInfoAPI
           :<|> Common.UnlinkVehicleAPI
           :<|> Common.EndRCAssociationAPI
           :<|> Common.AddVehicleAPI
           :<|> Common.SetRCStatusAPI
       )

-- driver cash collection api ----------------------------------------
-- have to write like that because in this case i have to store the dashboard used id for it. and which i am getting internally
type DriverCashCollectionAPI =
  Capture "driverId" (Id Common.Driver)
    :> "collectCash"
    :> ReqBody '[JSON] Text
    :> Post '[JSON] APISuccess

-------------------------------------

-- driver cash exemption api ----------------------------------------

type DriverCashExemptionAPI =
  Capture "driverId" (Id Common.Driver)
    :> "exemptCash"
    :> ReqBody '[JSON] Text
    :> Post '[JSON] APISuccess

----- payment history ----------
type DriverInfoAPI =
  "info"
    :> QueryParam "mobileNumber" Text
    :> QueryParam "mobileCountryCode" Text
    :> QueryParam "vehicleNumber" Text
    :> QueryParam "dlNumber" Text
    :> QueryParam "rcNumber" Text
    :> Capture "fleetOwnerId" Text
    :> Capture "mbFleet" Bool
    :> Get '[JSON] Common.DriverInfoRes

activateHandler :: ShortId DM.Merchant -> Context.City -> FlowServer ActivateAPI
activateHandler merchantId city =
  getDriverDue merchantId city
    :<|> enableDriver merchantId city
    :<|> collectCash merchantId city
    :<|> exemptCash merchantId city
    :<|> driverInfo merchantId city
    :<|> unlinkVehicle merchantId city
    :<|> endRCAssociation merchantId city
    :<|> addVehicle merchantId city
    :<|> setRCStatus merchantId city

getDriverDue :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Text -> FlowHandler [Common.DriverOutstandingBalanceResp]
getDriverDue merchantShortId opCity mobileCountryCode phone =
  withFlowHandlerAPI $ DDriver.getDriverDue merchantShortId opCity mobileCountryCode phone

enableDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
enableDriver merchantShortId opCity = withFlowHandlerAPI . DDriver.enableDriver merchantShortId opCity

collectCash :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> FlowHandler APISuccess
collectCash merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.collectCash merchantShortId opCity driverId

exemptCash :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> FlowHandler APISuccess
exemptCash merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.exemptCash merchantShortId opCity driverId

driverInfo :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Bool -> FlowHandler Common.DriverInfoRes
driverInfo merchantShortId opCity mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber mbRcNumber fleetOwnerId = withFlowHandlerAPI . DDriver.driverInfo merchantShortId opCity mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber mbRcNumber fleetOwnerId

unlinkVehicle :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
unlinkVehicle merchantShortId opCity = withFlowHandlerAPI . DDriver.unlinkVehicle merchantShortId opCity

endRCAssociation :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
endRCAssociation merchantShortId opCity = withFlowHandlerAPI . DDriver.endRCAssociation merchantShortId opCity

addVehicle :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.AddVehicleReq -> FlowHandler APISuccess
addVehicle merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.addVehicle merchantShortId opCity driverId

setRCStatus :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.RCStatusReq -> FlowHandler APISuccess
setRCStatus merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.setRCStatus merchantShortId opCity driverId
