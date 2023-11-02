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
import Kernel.Types.Beckn.Context as Context
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
           :<|> GetAllDriverForFleetAPI
           :<|> FleetUnlinkVehicleAPI
           :<|> FleetRemoveVehicleAPI
           :<|> FleetRemoveDriverAPI
           :<|> FleetTotalEarningAPI
           :<|> FleetVehicleEarningAPI
           :<|> FleetDriverEarningAPI
           :<|> Common.UpdateDriverNameAPI
           :<|> Common.SetRCStatusAPI
           :<|> Common.DeleteRCAPI
           :<|> Common.ClearOnRideStuckDriversAPI
           :<|> Common.GetDriverHomeLocationAPI
           :<|> Common.UpdateDriverHomeLocationAPI
           :<|> Common.IncrementDriverGoToCountAPI
           :<|> Common.GetDriverGoHomeInfoAPI
           :<|> DriverPaymentHistoryAPI
           :<|> DriverPaymentHistoryEntityDetailsAPI
           :<|> Common.UpdateSubscriptionDriverFeeAndInvoiceAPI
           :<|> GetFleetDriverVehicleAssociationAPI
           :<|> GetFleetDriverAssociationAPI
           :<|> GetFleetVehicleAssociationAPI
           :<|> SetVehicleDriverRcStatusForFleetAPI
           :<|> SendSmsToDriverAPI
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

-------------------------------------------------------------------
------- Send Sms to Driver ----------------------------------------

type SendMessageToDriverViaDashboardAPI =
  Capture "driverId" (Id Common.Driver)
    :> "sendSms"
    :> ReqBody '[JSON] DDriver.SendSmsReq
    :> Post '[JSON] APISuccess

type SendSmsToDriverAPI =
  Capture "driverId" (Id Common.Driver)
    :> Capture "volunteerId" Text
    :> "sendSms"
    :> ReqBody '[JSON] DDriver.SendSmsReq
    :> Post '[JSON] APISuccess

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
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
    :<|> getAllDriverForFleet merchantId city
    :<|> fleetUnlinkVehicle merchantId city
    :<|> fleetRemoveVehicle merchantId city
    :<|> fleetRemoveDriver merchantId city
    :<|> fleetTotalEarning merchantId city
    :<|> fleetVehicleEarning merchantId city
    :<|> fleetDriverEarning merchantId city
    :<|> updateDriverName merchantId city
    :<|> setRCStatus merchantId city
    :<|> deleteRC merchantId city
    :<|> clearOnRideStuckDrivers merchantId city
    :<|> getDriverHomeLocation merchantId city
    :<|> updateDriverHomeLocation merchantId city
    :<|> incrementDriverGoToCount merchantId city
    :<|> getDriverGoHomeInfo merchantId city
    :<|> getPaymentHistory merchantId city
    :<|> getPaymentHistoryEntityDetails merchantId city
    :<|> updateDriverSubscriptionDriverFeeAndInvoiceUpdate merchantId city
    :<|> getFleetDriverVehicleAssociation merchantId city
    :<|> getFleetDriverAssociation merchantId city
    :<|> getFleetVehicleAssociation merchantId city
    :<|> setVehicleDriverRcStatusForFleet merchantId city
    :<|> sendSmsToDriver merchantId city

driverDocumentsInfo :: ShortId DM.Merchant -> Context.City -> FlowHandler Common.DriverDocumentsInfoRes
driverDocumentsInfo merchantShortId = withFlowHandlerAPI . DDriver.driverDocumentsInfo merchantShortId

driverAadhaarInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler Common.DriverAadhaarInfoRes
driverAadhaarInfo merchantShortId opCity = withFlowHandlerAPI . DDriver.driverAadhaarInfo merchantShortId opCity

driverAadhaarInfoByPhone :: ShortId DM.Merchant -> Context.City -> Text -> FlowHandler Common.DriverAadhaarInfoByPhoneReq
driverAadhaarInfoByPhone merchantShortId opCity = withFlowHandlerAPI . DDriver.driverAadhaarInfoByPhone merchantShortId opCity

listDrivers :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> FlowHandler Common.DriverListRes
listDrivers merchantShortId opCity mbLimit mbOffset verified enabled blocked mbSubscribed vechicleNumberSearchString =
  withFlowHandlerAPI . DDriver.listDrivers merchantShortId opCity mbLimit mbOffset verified enabled blocked mbSubscribed vechicleNumberSearchString

getDriverDue :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Text -> FlowHandler [Common.DriverOutstandingBalanceResp]
getDriverDue merchantShortId opCity mobileCountryCode phone =
  withFlowHandlerAPI $ DDriver.getDriverDue merchantShortId opCity mobileCountryCode phone

driverActivity :: ShortId DM.Merchant -> Context.City -> FlowHandler Common.DriverActivityRes
driverActivity merchantShortId = withFlowHandlerAPI . DDriver.driverActivity merchantShortId

enableDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
enableDriver merchantShortId opCity = withFlowHandlerAPI . DDriver.enableDriver merchantShortId opCity

disableDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
disableDriver merchantShortId opCity = withFlowHandlerAPI . DDriver.disableDriver merchantShortId opCity

blockDriverWithReason :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.BlockDriverWithReasonReq -> FlowHandler APISuccess
blockDriverWithReason merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.blockDriverWithReason merchantShortId opCity driverId

blockDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
blockDriver merchantShortId opCity = withFlowHandlerAPI . DDriver.blockDriver merchantShortId opCity

blockReasonList :: ShortId DM.Merchant -> Context.City -> FlowHandler [Common.BlockReason]
blockReasonList _ _ = withFlowHandlerAPI DDriver.blockReasonList

collectCash :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> FlowHandler APISuccess
collectCash merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.collectCash merchantShortId opCity driverId

exemptCash :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> FlowHandler APISuccess
exemptCash merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.exemptCash merchantShortId opCity driverId

unblockDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
unblockDriver merchantShortId opCity = withFlowHandlerAPI . DDriver.unblockDriver merchantShortId opCity

driverLocation :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation merchantShortId opCity mbLimit mbOffset = withFlowHandlerAPI . DDriver.driverLocation merchantShortId opCity mbLimit mbOffset

driverInfo :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Bool -> FlowHandler Common.DriverInfoRes
driverInfo merchantShortId opCity mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber mbRcNumber fleetOwnerId = withFlowHandlerAPI . DDriver.driverInfo merchantShortId opCity mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber mbRcNumber fleetOwnerId

deleteDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
deleteDriver merchantShortId opCity = withFlowHandlerAPI . DDriver.deleteDriver merchantShortId opCity

unlinkVehicle :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
unlinkVehicle merchantShortId opCity = withFlowHandlerAPI . DDriver.unlinkVehicle merchantShortId opCity

unlinkDL :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
unlinkDL merchantShortId opCity = withFlowHandlerAPI . DDriver.unlinkDL merchantShortId opCity

unlinkAadhaar :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
unlinkAadhaar merchantShortId opCity = withFlowHandlerAPI . DDriver.unlinkAadhaar merchantShortId opCity

endRCAssociation :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
endRCAssociation merchantShortId opCity = withFlowHandlerAPI . DDriver.endRCAssociation merchantShortId opCity

updatePhoneNumber :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> FlowHandler APISuccess
updatePhoneNumber merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.updatePhoneNumber merchantShortId opCity driverId

updateByPhoneNumber :: ShortId DM.Merchant -> Context.City -> Text -> Common.UpdateDriverDataReq -> FlowHandler APISuccess
updateByPhoneNumber merchantShortId opCity mobileNo = withFlowHandlerAPI . DDriver.updateByPhoneNumber merchantShortId opCity mobileNo

addVehicle :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.AddVehicleReq -> FlowHandler APISuccess
addVehicle merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.addVehicle merchantShortId opCity driverId

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

updateDriverName :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateDriverNameReq -> FlowHandler APISuccess
updateDriverName merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.updateDriverName merchantShortId opCity driverId

clearOnRideStuckDrivers :: ShortId DM.Merchant -> Context.City -> Maybe Int -> FlowHandler Common.ClearOnRideStuckDriversRes
clearOnRideStuckDrivers merchantShortId opCity = withFlowHandlerAPI . DDriver.clearOnRideStuckDrivers merchantShortId opCity

getDriverHomeLocation :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler Common.GetHomeLocationsRes
getDriverHomeLocation merchantShortId opCity = withFlowHandlerAPI . DDriver.getDriverHomeLocation merchantShortId opCity

updateDriverHomeLocation :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateDriverHomeLocationReq -> FlowHandler APISuccess
updateDriverHomeLocation merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.updateDriverHomeLocation merchantShortId opCity driverId

incrementDriverGoToCount :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
incrementDriverGoToCount merchantShortId opCity = withFlowHandlerAPI . DDriver.incrementDriverGoToCount merchantShortId opCity

getDriverGoHomeInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler Common.CachedGoHomeRequestInfoRes
getDriverGoHomeInfo merchantShortId opCity = withFlowHandlerAPI . DDriver.getDriverGoHomeInfo merchantShortId opCity

setRCStatus :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.RCStatusReq -> FlowHandler APISuccess
setRCStatus merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.setRCStatus merchantShortId opCity driverId

deleteRC :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.DeleteRCReq -> FlowHandler APISuccess
deleteRC merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.deleteRC merchantShortId opCity driverId

getPaymentHistory :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Maybe INV.InvoicePaymentMode -> Maybe Int -> Maybe Int -> FlowHandler Driver.HistoryEntityV2
getPaymentHistory merchantShortId opCity driverId invoicePaymentMode limit offset = withFlowHandlerAPI $ DDriver.getPaymentHistory merchantShortId opCity driverId invoicePaymentMode limit offset

getPaymentHistoryEntityDetails :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Id INV.Invoice -> FlowHandler Driver.HistoryEntryDetailsEntityV2
getPaymentHistoryEntityDetails merchantShortId opCity driverId invoiceId = do
  withFlowHandlerAPI $ DDriver.getPaymentHistoryEntityDetails merchantShortId opCity driverId invoiceId

updateDriverSubscriptionDriverFeeAndInvoiceUpdate :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.SubscriptionDriverFeesAndInvoicesToUpdate -> FlowHandler Common.SubscriptionDriverFeesAndInvoicesToUpdate
updateDriverSubscriptionDriverFeeAndInvoiceUpdate merchantShortId opCity driverId req = withFlowHandlerAPI $ DDriver.updateSubscriptionDriverFeeAndInvoice merchantShortId opCity driverId req

setVehicleDriverRcStatusForFleet :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Common.RCStatusReq -> FlowHandler APISuccess
setVehicleDriverRcStatusForFleet merchantShortId opCity driverId fleetOwnerId req = withFlowHandlerAPI $ DDriver.setVehicleDriverRcStatusForFleet merchantShortId opCity driverId fleetOwnerId req

getFleetDriverVehicleAssociation :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> FlowHandler Common.DrivertoVehicleAssociationRes
getFleetDriverVehicleAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset = withFlowHandlerAPI $ DDriver.getFleetDriverVehicleAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset

getFleetDriverAssociation :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> FlowHandler Common.DrivertoVehicleAssociationRes
getFleetDriverAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset = withFlowHandlerAPI $ DDriver.getFleetDriverAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset

getFleetVehicleAssociation :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> FlowHandler Common.DrivertoVehicleAssociationRes
getFleetVehicleAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset = withFlowHandlerAPI $ DDriver.getFleetVehicleAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset

sendSmsToDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> DDriver.SendSmsReq -> FlowHandler APISuccess
sendSmsToDriver merchantShortId opCity driverId volunteerId = withFlowHandlerAPI . DDriver.sendSmsToDriver merchantShortId opCity driverId volunteerId
