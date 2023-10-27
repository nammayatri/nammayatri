{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Driver where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Action.UI.Driver as Driver
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (throwError)

type API =
  "driver"
    :> ( Common.DriverDocumentsInfoAPI
           :<|> Common.DriverAadhaarInfoAPI
           :<|> Common.DriverAadhaarInfoByPhoneAPI
           :<|> Common.DriverListAPI
           :<|> Common.DriverOutstandingBalanceAPI
           :<|> Common.DriverActivityAPI
           :<|> Common.EnableDriverAPI
           :<|> Common.DisableDriverAPI
           :<|> Common.BlockDriverWithReasonAPI
           :<|> Common.BlockDriverAPI
           :<|> Common.DriverBlockReasonListAPI
           :<|> DriverCashCollectionAPI
           :<|> DriverCashExemptionAPI
           :<|> Common.UnblockDriverAPI
           :<|> Common.DriverLocationAPI
           :<|> DriverInfoAPI
           :<|> Common.DeleteDriverAPI
           :<|> Common.UnlinkVehicleAPI
           :<|> Common.UnlinkDLAPI
           :<|> Common.UnlinkAadhaarAPI
           :<|> Common.EndRCAssociationAPI
           :<|> Common.UpdatePhoneNumberAPI
           :<|> Common.UpdateDriverAadhaarAPI
           :<|> Common.AddVehicleAPI
           :<|> AddVehicleForFleetAPI
           :<|> GetAllVehicleForFleetAPI
           :<|> FleetUnlinkVehicleAPI
           :<|> FleetRemoveVehicleAPI
           :<|> FleetStatsAPI
           :<|> Common.UpdateDriverNameAPI
           :<|> Common.SetRCStatusAPI
           :<|> Common.DeleteRCAPI
           :<|> Common.ClearOnRideStuckDriversAPI
           :<|> Common.GetDriverHomeLocationAPI
           :<|> Common.UpdateDriverHomeLocationAPI
           :<|> Common.IncrementDriverGoToCountAPI
           :<|> DriverPaymentHistoryAPI
           :<|> DriverPaymentHistoryEntityDetailsAPI
           :<|> Common.UpdateSubscriptionDriverFeeAndInvoiceAPI
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
type DriverPaymentHistoryAPI =
  Capture "driverId" (Id Common.Driver)
    :> "payments"
    :> "history"
    :> QueryParam "paymentMode" INV.InvoicePaymentMode
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] Driver.HistoryEntityV2

----------- payment history entry  -------------
type DriverPaymentHistoryEntityDetailsAPI =
  Capture "driverId" (Id Common.Driver)
    :> "payments"
    :> "history"
    :> Capture "invoiceId" (Id INV.Invoice)
    :> "entity"
    :> Get '[JSON] Driver.HistoryEntryDetailsEntityV2

-----------------------------------

-- --- add vehicle for driver api so here  we are passing the fleet owner api----

type AddVehicleForFleetAPI =
  Capture "mobileNo" Text
    :> QueryParam "mobileCountryCode" Text
    :> Capture "fleetOwnerId" Text
    :> "addVehicle"
    :> "fleet"
    :> ReqBody '[JSON] Common.AddVehicleReq
    :> Post '[JSON] APISuccess

-- --- add vehicle for driver api so here  we are passing the fleet owner api----

type GetAllVehicleForFleetAPI =
  Capture "fleetOwnerId" Text
    :> "getAllVehicle"
    :> "fleet"
    :> Get '[JSON] Common.ListVehicleRes

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

type FleetUnlinkVehicleAPI =
  Capture "fleetOwnerId" Text
    :> Capture "vehicleNo" Text
    :> QueryParam "mobileCountryCode" Text
    :> Capture "driverMobileNo" Text
    :> "unlink"
    :> "fleet"
    :> Post '[JSON] APISuccess

type FleetRemoveVehicleAPI =
  Capture "fleetOwnerId" Text
    :> Capture "vehicleNo" Text
    :> "remove"
    :> "fleet"
    :> Post '[JSON] APISuccess

type FleetStatsAPI =
  Capture "fleetOwnerId" Text
    :> "stats"
    :> "fleet"
    :> Get '[JSON] Common.FleetStatsRes

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  driverDocumentsInfo merchantId city
    :<|> driverAadhaarInfo merchantId city
    :<|> driverAadhaarInfoByPhone merchantId city
    :<|> listDrivers merchantId city
    :<|> getDriverDue merchantId city
    :<|> driverActivity merchantId city
    :<|> enableDriver merchantId city
    :<|> disableDriver merchantId city
    :<|> blockDriverWithReason merchantId city
    :<|> blockDriver merchantId city
    :<|> blockReasonList merchantId city
    :<|> collectCash merchantId city
    :<|> exemptCash merchantId city
    :<|> unblockDriver merchantId city
    :<|> driverLocation merchantId city
    :<|> driverInfo merchantId city
    :<|> deleteDriver merchantId city
    :<|> unlinkVehicle merchantId city
    :<|> unlinkDL merchantId city
    :<|> unlinkAadhaar merchantId city
    :<|> endRCAssociation merchantId city
    :<|> updatePhoneNumber merchantId city
    :<|> updateByPhoneNumber merchantId city
    :<|> addVehicle merchantId city
    :<|> addVehicleForFleet merchantId city
    :<|> getAllVehicleForFleet merchantId city
    :<|> fleetUnlinkVehicle merchantId city
    :<|> fleetRemoveVehicle merchantId city
    :<|> fleetStats merchantId city
    :<|> updateDriverName merchantId city
    :<|> setRCStatus merchantId city
    :<|> deleteRC merchantId city
    :<|> clearOnRideStuckDrivers merchantId city
    :<|> getDriverHomeLocation merchantId city
    :<|> updateDriverHomeLocation merchantId city
    :<|> incrementDriverGoToCount merchantId city
    :<|> getPaymentHistory merchantId city
    :<|> getPaymentHistoryEntityDetails merchantId city
    :<|> updateDriverSubscriptionDriverFeeAndInvoiceUpdate merchantId city

driverDocumentsInfo :: ShortId DM.Merchant -> City.City -> FlowHandler Common.DriverDocumentsInfoRes
driverDocumentsInfo merchantShortId = withFlowHandlerAPI . DDriver.driverDocumentsInfo merchantShortId

driverAadhaarInfo :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> FlowHandler Common.DriverAadhaarInfoRes
driverAadhaarInfo merchantShortId opCity = withFlowHandlerAPI . DDriver.driverAadhaarInfo merchantShortId opCity

driverAadhaarInfoByPhone :: ShortId DM.Merchant -> City.City -> Text -> FlowHandler Common.DriverAadhaarInfoByPhoneReq
driverAadhaarInfoByPhone merchantShortId opCity = withFlowHandlerAPI . DDriver.driverAadhaarInfoByPhone merchantShortId opCity

listDrivers :: ShortId DM.Merchant -> City.City -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> FlowHandler Common.DriverListRes
listDrivers merchantShortId opCity mbLimit mbOffset verified enabled blocked mbSubscribed vechicleNumberSearchString =
  withFlowHandlerAPI . DDriver.listDrivers merchantShortId opCity mbLimit mbOffset verified enabled blocked mbSubscribed vechicleNumberSearchString

getDriverDue :: ShortId DM.Merchant -> City.City -> Maybe Text -> Text -> FlowHandler [Common.DriverOutstandingBalanceResp]
getDriverDue merchantShortId opCity mobileCountryCode phone =
  withFlowHandlerAPI $ DDriver.getDriverDue merchantShortId opCity mobileCountryCode phone

driverActivity :: ShortId DM.Merchant -> City.City -> FlowHandler Common.DriverActivityRes
driverActivity merchantShortId = withFlowHandlerAPI . DDriver.driverActivity merchantShortId

enableDriver :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> FlowHandler APISuccess
enableDriver merchantShortId opCity = withFlowHandlerAPI . DDriver.enableDriver merchantShortId opCity

disableDriver :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> FlowHandler APISuccess
disableDriver merchantShortId opCity = withFlowHandlerAPI . DDriver.disableDriver merchantShortId opCity

blockDriverWithReason :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> Common.BlockDriverWithReasonReq -> FlowHandler APISuccess
blockDriverWithReason merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.blockDriverWithReason merchantShortId opCity driverId

blockDriver :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> FlowHandler APISuccess
blockDriver merchantShortId opCity = withFlowHandlerAPI . DDriver.blockDriver merchantShortId opCity

blockReasonList :: ShortId DM.Merchant -> City.City -> FlowHandler [Common.BlockReason]
blockReasonList _ _ = withFlowHandlerAPI DDriver.blockReasonList

collectCash :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> Text -> FlowHandler APISuccess
collectCash merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.collectCash merchantShortId opCity driverId

exemptCash :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> Text -> FlowHandler APISuccess
exemptCash merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.exemptCash merchantShortId opCity driverId

unblockDriver :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> FlowHandler APISuccess
unblockDriver merchantShortId opCity = withFlowHandlerAPI . DDriver.unblockDriver merchantShortId opCity

driverLocation :: ShortId DM.Merchant -> City.City -> Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation merchantShortId opCity mbLimit mbOffset = withFlowHandlerAPI . DDriver.driverLocation merchantShortId opCity mbLimit mbOffset

driverInfo :: ShortId DM.Merchant -> City.City -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Bool -> FlowHandler Common.DriverInfoRes
driverInfo merchantShortId opCity mbMobileNumber mbMobileCountryCode mbVehicleNumber mbRcNumber mbDlNumber fleetOwnerId = withFlowHandlerAPI . DDriver.driverInfo merchantShortId opCity mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber mbRcNumber fleetOwnerId

deleteDriver :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> FlowHandler APISuccess
deleteDriver merchantShortId opCity = withFlowHandlerAPI . DDriver.deleteDriver merchantShortId opCity

unlinkVehicle :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> FlowHandler APISuccess
unlinkVehicle merchantShortId opCity = withFlowHandlerAPI . DDriver.unlinkVehicle merchantShortId opCity

unlinkDL :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> FlowHandler APISuccess
unlinkDL merchantShortId opCity = withFlowHandlerAPI . DDriver.unlinkDL merchantShortId opCity

unlinkAadhaar :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> FlowHandler APISuccess
unlinkAadhaar merchantShortId opCity = withFlowHandlerAPI . DDriver.unlinkAadhaar merchantShortId opCity

endRCAssociation :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> FlowHandler APISuccess
endRCAssociation merchantShortId opCity = withFlowHandlerAPI . DDriver.endRCAssociation merchantShortId opCity

updatePhoneNumber :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> FlowHandler APISuccess
updatePhoneNumber merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.updatePhoneNumber merchantShortId opCity driverId

updateByPhoneNumber :: ShortId DM.Merchant -> City.City -> Text -> Common.UpdateDriverDataReq -> FlowHandler APISuccess
updateByPhoneNumber merchantShortId opCity mobileNo = withFlowHandlerAPI . DDriver.updateByPhoneNumber merchantShortId opCity mobileNo

addVehicle :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> Common.AddVehicleReq -> FlowHandler APISuccess
addVehicle merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.addVehicle merchantShortId opCity driverId

addVehicleForFleet :: ShortId DM.Merchant -> City.City -> Text -> Maybe Text -> Text -> Common.AddVehicleReq -> FlowHandler APISuccess
addVehicleForFleet merchantShortId opCity phoneNo mbMobileCountryCode fleetOwnerId = withFlowHandlerAPI . DDriver.addVehicleForFleet merchantShortId opCity phoneNo mbMobileCountryCode fleetOwnerId

getAllVehicleForFleet :: ShortId DM.Merchant -> City.City -> Text -> FlowHandler Common.ListVehicleRes
getAllVehicleForFleet merchantShortId opCity = withFlowHandlerAPI . DDriver.getAllVehicleForFleet merchantShortId opCity

fleetUnlinkVehicle :: ShortId DM.Merchant -> City.City -> Text -> Text -> Maybe Text -> Text -> FlowHandler APISuccess
fleetUnlinkVehicle merchantShortId opCity fleetOwnerId vehicleNo mbMobileCountryCode = withFlowHandlerAPI . DDriver.fleetUnlinkVehicle merchantShortId opCity fleetOwnerId vehicleNo mbMobileCountryCode

fleetRemoveVehicle :: ShortId DM.Merchant -> City.City -> Text -> Text -> FlowHandler APISuccess
fleetRemoveVehicle merchantShortId opCity fleetOwnerId = withFlowHandlerAPI . DDriver.fleetRemoveVehicle merchantShortId opCity fleetOwnerId

fleetStats :: ShortId DM.Merchant -> City.City -> Text -> FlowHandler Common.FleetStatsRes
fleetStats merchantShortId opCity = withFlowHandlerAPI . DDriver.fleetStats merchantShortId opCity

updateDriverName :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> Common.UpdateDriverNameReq -> FlowHandler APISuccess
updateDriverName merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.updateDriverName merchantShortId opCity driverId

clearOnRideStuckDrivers :: ShortId DM.Merchant -> City.City -> Maybe Int -> FlowHandler Common.ClearOnRideStuckDriversRes
clearOnRideStuckDrivers merchantShortId opCity = withFlowHandlerAPI . DDriver.clearOnRideStuckDrivers merchantShortId opCity

getDriverHomeLocation :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> FlowHandler Common.GetHomeLocationsRes
getDriverHomeLocation merchantShortId opCity = withFlowHandlerAPI . DDriver.getDriverHomeLocation merchantShortId opCity

updateDriverHomeLocation :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> Common.UpdateDriverHomeLocationReq -> FlowHandler APISuccess
updateDriverHomeLocation merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.updateDriverHomeLocation merchantShortId opCity driverId

incrementDriverGoToCount :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> FlowHandler APISuccess
incrementDriverGoToCount merchantShortId opCity = withFlowHandlerAPI . DDriver.incrementDriverGoToCount merchantShortId opCity

setRCStatus :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> Common.RCStatusReq -> FlowHandler APISuccess
setRCStatus merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.setRCStatus merchantShortId opCity driverId

deleteRC :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> Common.DeleteRCReq -> FlowHandler APISuccess
deleteRC merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.deleteRC merchantShortId opCity driverId

getPaymentHistory :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> Maybe INV.InvoicePaymentMode -> Maybe Int -> Maybe Int -> FlowHandler Driver.HistoryEntityV2
getPaymentHistory merchantShortId opCity driverId invoicePaymentMode limit offset = withFlowHandlerAPI $ DDriver.getPaymentHistory merchantShortId opCity driverId invoicePaymentMode limit offset

getPaymentHistoryEntityDetails :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> Id INV.Invoice -> FlowHandler Driver.HistoryEntryDetailsEntityV2
getPaymentHistoryEntityDetails merchantShortId opCity driverId invoiceId = do
  withFlowHandlerAPI $ DDriver.getPaymentHistoryEntityDetails merchantShortId opCity driverId invoiceId

updateDriverSubscriptionDriverFeeAndInvoiceUpdate :: ShortId DM.Merchant -> City.City -> Id Common.Driver -> Common.SubscriptionDriverFeesAndInvoicesToUpdate -> FlowHandler Common.SubscriptionDriverFeesAndInvoicesToUpdate
updateDriverSubscriptionDriverFeeAndInvoiceUpdate merchantShortId opCity driverId req = withFlowHandlerAPI $ DDriver.updateSubscriptionDriverFeeAndInvoice merchantShortId opCity driverId req
