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

-- import qualified "dynamic-offer-driver-app" Domain.Types.Invoice as INV

import qualified "dynamic-offer-driver-app" API.Dashboard.Driver as ADDriver
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Driver as Driver
import qualified "dynamic-offer-driver-app" Domain.Types.Invoice as INV
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Role as DRole
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, fromMaybeM, throwError, withFlowHandlerAPI)
import Kernel.Utils.Validation (runRequestValidation)
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Storage.Queries.Person as QP
import "lib-dashboard" Storage.Queries.Role as QRole
import "lib-dashboard" Tools.Auth hiding (BECKN_TRANSPORT)
import "lib-dashboard" Tools.Auth.Merchant
import "lib-dashboard" Tools.Error

type API =
  "driver"
    :> ( DriverDocumentsInfoAPI
           :<|> DriverAadhaarInfoAPI
           :<|> DriverAadhaarInfoByPhoneAPI
           :<|> DriverListAPI
           :<|> DriverOutstandingBalanceAPI
           :<|> DriverActivityAPI
           :<|> EnableDriverAPI
           :<|> DisableDriverAPI
           :<|> BlockDriverWithReasonAPI
           :<|> BlockDriverAPI
           :<|> BlockReasonListAPI
           :<|> DriverCashCollectionAPI
           :<|> DriverCashExemptionAPI
           :<|> UnblockDriverAPI
           :<|> DriverLocationAPI
           :<|> DriverInfoAPI
           :<|> DeleteDriverAPI
           :<|> UnlinkVehicleAPI
           :<|> UnlinkDLAPI
           :<|> UnlinkAadhaarAPI
           :<|> EndRCAssociationAPI
           :<|> UpdatePhoneNumberAPI
           :<|> UpdateDriverAadhaarAPI
           :<|> AddVehicleAPI
           :<|> AddVehicleForFleetAPI
           :<|> GetAllVehicleForFleetAPI
           :<|> FleetUnlinkVehicleAPI
           :<|> FleetRemoveVehicleAPI
           :<|> FleetStatsAPI
           :<|> UpdateDriverNameAPI
           :<|> SetRCStatusAPI
           :<|> DeleteRCAPI
           :<|> SyncRCAPI
           :<|> ClearOnRideStuckDriversAPI
           :<|> GetDriverHomeLocationAPI
           :<|> UpdateDriverHomeLocationAPI
           :<|> IncrementDriverGoToCountAPI
           :<|> DriverPaymentHistoryAPI
           :<|> DriverPaymentHistoryEntityDetailsAPI
       )

type DriverDocumentsInfoAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'DOCUMENTS_INFO
    :> Common.DriverDocumentsInfoAPI

type DriverAadhaarInfoAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'AADHAAR_INFO
    :> Common.DriverAadhaarInfoAPI

type DriverAadhaarInfoByPhoneAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'AADHAAR_INFO_PHONE
    :> Common.DriverAadhaarInfoByPhoneAPI

type DriverListAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'LIST
    :> Common.DriverListAPI

type DriverOutstandingBalanceAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'BALANCE_DUE
    :> Common.DriverOutstandingBalanceAPI

type DriverCashCollectionAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'COLLECT_CASH
    :> Common.DriverCashCollectionAPI

type DriverCashExemptionAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'EXEMPT_CASH
    :> Common.DriverCashExemptionAPI

type DriverActivityAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'ACTIVITY
    :> Common.DriverActivityAPI

type EnableDriverAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'ENABLE
    :> Common.EnableDriverAPI

type DisableDriverAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'DISABLE
    :> Common.DisableDriverAPI

type BlockDriverWithReasonAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'BLOCK_WITH_REASON
    :> Common.BlockDriverWithReasonAPI

type BlockDriverAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'BLOCK
    :> Common.BlockDriverAPI

type BlockReasonListAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'BLOCK_REASON_LIST
    :> Common.DriverBlockReasonListAPI

type UnblockDriverAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'UNBLOCK
    :> Common.UnblockDriverAPI

type DriverLocationAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'LOCATION
    :> Common.DriverLocationAPI

type DriverInfoAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'INFO
    :> Common.DriverInfoAPI

type DeleteDriverAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'DELETE_DRIVER
    :> Common.DeleteDriverAPI

type UnlinkVehicleAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'UNLINK_VEHICLE
    :> Common.UnlinkVehicleAPI

type EndRCAssociationAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'END_RC_ASSOCIATION
    :> Common.EndRCAssociationAPI

type SetRCStatusAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'SET_RC_STATUS
    :> Common.SetRCStatusAPI

type DeleteRCAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'DELETE_RC
    :> Common.DeleteRCAPI

type SyncRCAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'SYNC_RC
    :> Common.SyncRCAPI

type UnlinkDLAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'UNLINK_DL
    :> Common.UnlinkDLAPI

type UnlinkAadhaarAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'UNLINK_AADHAAR
    :> Common.UnlinkAadhaarAPI

type UpdatePhoneNumberAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'UPDATE_PHONE_NUMBER
    :> Common.UpdatePhoneNumberAPI

type UpdateDriverAadhaarAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'AADHAAR_UPDATE
    :> Common.UpdateDriverAadhaarAPI

type AddVehicleAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'ADD_VEHICLE
    :> Common.AddVehicleAPI

type AddVehicleForFleetAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'FLEET 'ADD_VEHICLE_FLEET
    :> Common.AddVehicleForFleetAPI

type GetAllVehicleForFleetAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'FLEET 'GET_ALL_VEHICLE_FOR_FLEET
    :> Common.GetAllVehicleForFleetAPI

type FleetUnlinkVehicleAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'FLEET 'FLEET_UNLINK_VEHICLE
    :> Common.FleetUnlinkVehicleAPI

type FleetRemoveVehicleAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'FLEET 'FLEET_REMOVE_VEHICLE
    :> Common.FleetRemoveVehicleAPI

type FleetStatsAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'FLEET 'FLEET_STATS
    :> Common.FleetStatsAPI

type UpdateDriverNameAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'UPDATE_DRIVER_NAME
    :> Common.UpdateDriverNameAPI

type ClearOnRideStuckDriversAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'CLEAR_ON_RIDE_STUCK_DRIVER_IDS
    :> Common.ClearOnRideStuckDriversAPI

type GetDriverHomeLocationAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'GET_DRIVER_HOME_LOCATION
    :> Common.GetDriverHomeLocationAPI

type UpdateDriverHomeLocationAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'UPDATE_DRIVER_HOME_LOCATION
    :> Common.UpdateDriverHomeLocationAPI

type IncrementDriverGoToCountAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'INCREMENT_DRIVER_GO_TO_COUNT
    :> Common.IncrementDriverGoToCountAPI

type DriverPaymentHistoryAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'PAYMENT_HISTORY
    :> ADDriver.DriverPaymentHistoryAPI

type DriverPaymentHistoryEntityDetailsAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'PAYMENT_HISTORY_ENTITY_DETAILS
    :> ADDriver.DriverPaymentHistoryEntityDetailsAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  driverDocuments merchantId
    :<|> driverAadhaarInfo merchantId
    :<|> driverAadhaarInfoByPhone merchantId
    :<|> listDriver merchantId
    :<|> getDriverDue merchantId
    :<|> driverActivity merchantId
    :<|> enableDriver merchantId
    :<|> disableDriver merchantId
    :<|> blockDriverWithReason merchantId
    :<|> blockDriver merchantId
    :<|> blockReasonList merchantId
    :<|> collectCash merchantId
    :<|> exemptCash merchantId
    :<|> unblockDriver merchantId
    :<|> driverLocation merchantId
    :<|> driverInfo merchantId
    :<|> deleteDriver merchantId
    :<|> unlinkVehicle merchantId
    :<|> unlinkDL merchantId
    :<|> unlinkAadhaar merchantId
    :<|> endRCAssociation merchantId
    :<|> updatePhoneNumber merchantId
    :<|> updateByPhoneNumber merchantId
    :<|> addVehicle merchantId
    :<|> addVehicleForFleet merchantId
    :<|> getAllVehicleForFleet merchantId
    :<|> fleetUnlinkVehicle merchantId
    :<|> fleetRemoveVehicle merchantId
    :<|> fleetStats merchantId
    :<|> updateDriverName merchantId
    :<|> setRCStatus merchantId
    :<|> deleteRC merchantId
    :<|> syncRC merchantId
    :<|> clearOnRideStuckDrivers merchantId
    :<|> getDriverHomeLocation merchantId
    :<|> updateDriverHomeLocation merchantId
    :<|> incrementDriverGoToCount merchantId
    :<|> getPaymentHistory merchantId
    :<|> getPaymentHistoryEntityDetails merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.DriverEndpoint ->
  ApiTokenInfo ->
  Id Common.Driver ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo driverId =
  T.buildTransaction (DT.DriverAPI endpoint) (Just DRIVER_OFFER_BPP) (Just apiTokenInfo) (Just driverId) Nothing

driverDocuments :: ShortId DM.Merchant -> ApiTokenInfo -> FlowHandler Common.DriverDocumentsInfoRes
driverDocuments merchantShortId apiTokenInfo = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.driverDocumentsInfo)

driverAadhaarInfo :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler Common.DriverAadhaarInfoRes
driverAadhaarInfo merchantShortId apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.driverAadhaarInfo) driverId

driverAadhaarInfoByPhone :: ShortId DM.Merchant -> ApiTokenInfo -> Text -> FlowHandler Common.DriverAadhaarInfoByPhoneReq
driverAadhaarInfoByPhone merchantShortId apiTokenInfo phoneNo = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.driverAadhaarInfoByPhone) phoneNo

listDriver :: ShortId DM.Merchant -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> FlowHandler Common.DriverListRes
listDriver merchantShortId apiTokenInfo mbLimit mbOffset verified enabled blocked mbSubscribed phone mbVehicleNumberSearchString = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.listDrivers) mbLimit mbOffset verified enabled blocked mbSubscribed phone mbVehicleNumberSearchString

getDriverDue :: ShortId DM.Merchant -> ApiTokenInfo -> Maybe Text -> Text -> FlowHandler [Common.DriverOutstandingBalanceResp]
getDriverDue merchantShortId apiTokenInfo mbMobileCountryCode phone = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.getDriverDue) mbMobileCountryCode phone

driverActivity :: ShortId DM.Merchant -> ApiTokenInfo -> FlowHandler Common.DriverActivityRes
driverActivity merchantShortId apiTokenInfo = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.driverActivity)

collectCash :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
collectCash merchantShortId apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.CollectCashEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.collectCash) driverId apiTokenInfo.personId.getId

exemptCash :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
exemptCash merchantShortId apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.ExemptCashEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.exemptCash) driverId apiTokenInfo.personId.getId

enableDriver :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
enableDriver merchantShortId apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.EnableDriverEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.enableDriver) driverId

disableDriver :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
disableDriver merchantShortId apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.DisableDriverEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.disableDriver) driverId

blockDriverWithReason :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> Common.BlockDriverWithReasonReq -> FlowHandler APISuccess
blockDriverWithReason merchantShortId apiTokenInfo driverId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.BlockDriverWithReasonEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.blockDriverWithReason) driverId req

blockDriver :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
blockDriver merchantShortId apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.BlockDriverEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.blockDriver) driverId

blockReasonList :: ShortId DM.Merchant -> ApiTokenInfo -> FlowHandler [Common.BlockReason]
blockReasonList merchantShortId apiTokenInfo = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.blockReasonList)

unblockDriver :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
unblockDriver merchantShortId apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.UnblockDriverEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.unblockDriver) driverId

driverLocation :: ShortId DM.Merchant -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation merchantShortId apiTokenInfo mbLimit mbOffset req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.driverLocation) mbLimit mbOffset req

driverInfo ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  FlowHandler Common.DriverInfoRes
driverInfo merchantShortId apiTokenInfo mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber mbRcNumber = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  unless (length (catMaybes [mbMobileNumber, mbVehicleNumber, mbDlNumber, mbRcNumber]) == 1) $
    throwError $ InvalidRequest "Exactly one of query parameters \"mobileNumber\", \"vehicleNumber\", \"dlNumber\", \"rcNumber\" is required"
  when (isJust mbMobileCountryCode && isNothing mbMobileNumber) $
    throwError $ InvalidRequest "\"mobileCountryCode\" can be used only with \"mobileNumber\""
  encPerson <- QP.findById apiTokenInfo.personId >>= fromMaybeM (PersonNotFound apiTokenInfo.personId.getId)
  role <- QRole.findById encPerson.roleId >>= fromMaybeM (RoleNotFound encPerson.roleId.getId)
  let mbFleet = role.dashboardAccessType == DRole.FLEET_OWNER
  Client.callDriverOfferBPP checkedMerchantId (.drivers.driverInfo) mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber mbRcNumber apiTokenInfo.personId.getId mbFleet

deleteDriver :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
deleteDriver merchantShortId apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.DeleteDriverEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.deleteDriver) driverId

unlinkVehicle :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
unlinkVehicle merchantShortId apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.UnlinkVehicleEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.unlinkVehicle) driverId

updatePhoneNumber :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> FlowHandler APISuccess
updatePhoneNumber merchantShortId apiTokenInfo driverId req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateUpdatePhoneNumberReq req
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.UpdatePhoneNumberEndpoint apiTokenInfo driverId $ Just req
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.updatePhoneNumber) driverId req

updateByPhoneNumber :: ShortId DM.Merchant -> ApiTokenInfo -> Text -> Common.UpdateDriverDataReq -> FlowHandler APISuccess
updateByPhoneNumber merchantShortId apiTokenInfo phoneNo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.updateByPhoneNumber) phoneNo req

addVehicle :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> Common.AddVehicleReq -> FlowHandler APISuccess
addVehicle merchantShortId apiTokenInfo driverId req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateAddVehicleReq req
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.AddVehicleEndpoint apiTokenInfo driverId $ Just req
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.addVehicle) driverId req

addVehicleForFleet :: ShortId DM.Merchant -> ApiTokenInfo -> Text -> Maybe Text -> Common.AddVehicleReq -> FlowHandler APISuccess
addVehicleForFleet merchantShortId apiTokenInfo phoneNo mbMobileCountryCode req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateAddVehicleReq req
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.addVehicleForFleet) phoneNo mbMobileCountryCode apiTokenInfo.personId.getId req

getAllVehicleForFleet :: ShortId DM.Merchant -> ApiTokenInfo -> FlowHandler Common.ListVehicleRes
getAllVehicleForFleet merchantShortId apiTokenInfo = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.getAllVehicleForFleet) apiTokenInfo.personId.getId

fleetUnlinkVehicle :: ShortId DM.Merchant -> ApiTokenInfo -> Text -> Maybe Text -> Text -> FlowHandler APISuccess
fleetUnlinkVehicle merchantShortId apiTokenInfo vehicleNo mbMobileCountryCode mobileNo = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.fleetUnlinkVehicle) apiTokenInfo.personId.getId vehicleNo mbMobileCountryCode mobileNo

fleetRemoveVehicle :: ShortId DM.Merchant -> ApiTokenInfo -> Text -> FlowHandler APISuccess
fleetRemoveVehicle merchantShortId apiTokenInfo vehicleNo = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.fleetRemoveVehicle) apiTokenInfo.personId.getId vehicleNo

fleetStats :: ShortId DM.Merchant -> ApiTokenInfo -> FlowHandler Common.FleetStatsRes
fleetStats merchantShortId apiTokenInfo = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.fleetStats) apiTokenInfo.personId.getId

updateDriverName :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> Common.UpdateDriverNameReq -> FlowHandler APISuccess
updateDriverName merchantShortId apiTokenInfo driverId req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateUpdateDriverNameReq req
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.UpdateDriverNameEndpoint apiTokenInfo driverId $ Just req
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.updateDriverName) driverId req

unlinkDL :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
unlinkDL merchantShortId apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.UnlinkDLEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.unlinkDL) driverId

unlinkAadhaar :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
unlinkAadhaar merchantShortId apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.UnlinkAadhaarEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.unlinkAadhaar) driverId

endRCAssociation :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
endRCAssociation merchantShortId apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.EndRCAssociationEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.endRCAssociation) driverId

setRCStatus :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> Common.RCStatusReq -> FlowHandler APISuccess
setRCStatus merchantShortId apiTokenInfo driverId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.SetRCStatusEndpoint apiTokenInfo driverId $ Just req
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.setRCStatus) driverId req

deleteRC :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> Common.DeleteRCReq -> FlowHandler APISuccess
deleteRC merchantShortId apiTokenInfo driverId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.DeleteRCEndpoint apiTokenInfo driverId $ Just req
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.deleteRC) driverId req

syncRC :: ShortId DM.Merchant -> ApiTokenInfo -> Common.SyncRCReq -> FlowHandler APISuccess
syncRC merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId -- should we store transaction without driverId?
  Client.callDriverOfferBPP checkedMerchantId (.drivers.syncRC) req

clearOnRideStuckDrivers :: ShortId DM.Merchant -> ApiTokenInfo -> Maybe Int -> FlowHandler Common.ClearOnRideStuckDriversRes
clearOnRideStuckDrivers merchantShortId apiTokenInfo dbSyncTime = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.clearOnRideStuckDrivers) dbSyncTime

getDriverHomeLocation :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler Common.GetHomeLocationsRes
getDriverHomeLocation merchantShortId apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.getDriverHomeLocation) driverId

updateDriverHomeLocation :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> Common.UpdateDriverHomeLocationReq -> FlowHandler APISuccess
updateDriverHomeLocation merchantShortId apiTokenInfo driverId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.UpdateDriverHomeLocationEndpoint apiTokenInfo driverId $ Just req
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.updateDriverHomeLocation) driverId req

incrementDriverGoToCount :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
incrementDriverGoToCount merchantShortId apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.IncrementDriverGoToCountEndPoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.drivers.incrementDriverGoToCount) driverId

getPaymentHistory :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> Maybe INV.InvoicePaymentMode -> Maybe Int -> Maybe Int -> FlowHandler Driver.HistoryEntityV2
getPaymentHistory merchantShortId apiTokenInfo driverId paymentMode limit offset = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.getPaymentHistory) driverId paymentMode limit offset

getPaymentHistoryEntityDetails :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> Id INV.Invoice -> FlowHandler Driver.HistoryEntryDetailsEntityV2
getPaymentHistoryEntityDetails merchantShortId apiTokenInfo driverId invoiceId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.getPaymentHistoryEntityDetails) driverId invoiceId
