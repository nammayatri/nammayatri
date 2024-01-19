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

import qualified "dynamic-offer-driver-app" API.Dashboard.Management.Subscription as ADSubscription
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Driver as DDriver
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Driver as Driver
import qualified "dynamic-offer-driver-app" Domain.Types.Invoice as INV
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Role as DRole
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, fromMaybeM, throwError, withFlowHandlerAPI')
import Kernel.Utils.Validation (runRequestValidation)
import qualified ProviderPlatformClient.DynamicOfferDriver.Fleet as Client
import qualified ProviderPlatformClient.DynamicOfferDriver.Operations as Client
import qualified ProviderPlatformClient.DynamicOfferDriver.RideBooking as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Person as QP
import "lib-dashboard" Storage.Queries.Role as QRole
import "lib-dashboard" Tools.Auth
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
           :<|> GetAllDriverForFleetAPI
           :<|> FleetUnlinkVehicleAPI
           :<|> FleetRemoveVehicleAPI
           :<|> FleetRemoveDriverAPI
           :<|> FleetTotalEarningAPI
           :<|> FleetVehicleEarningAPI
           :<|> FleetDriverEarningAPI
           :<|> UpdateDriverNameAPI
           :<|> SetRCStatusAPI
           :<|> DeleteRCAPI
           :<|> ClearOnRideStuckDriversAPI
           :<|> GetDriverHomeLocationAPI
           :<|> UpdateDriverHomeLocationAPI
           :<|> IncrementDriverGoToCountAPI
           :<|> GetDriverGoHomeInfoAPI
           :<|> DriverPaymentHistoryAPI
           :<|> DriverPaymentHistoryEntityDetailsAPI
           :<|> DriverSubscriptionDriverFeeAndInvoiceUpdateAPI
           :<|> GetFleetDriverVehicleAssociationAPI
           :<|> GetFleetDriverAssociationAPI
           :<|> GetFleetVehicleAssociationAPI
           :<|> SetVehicleDriverRcStatusForFleetAPI
           :<|> SendMessageToDriverViaDashboardAPI
           :<|> SendDummyRideRequestToDriverViaDashboardAPI
           :<|> ChangeOperatingCityAPI
       )

type DriverDocumentsInfoAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'DOCUMENTS_INFO
    :> Common.DriverDocumentsInfoAPI

type DriverAadhaarInfoAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'AADHAAR_INFO
    :> Common.DriverAadhaarInfoAPI

type DriverAadhaarInfoByPhoneAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'AADHAAR_INFO_PHONE
    :> Common.DriverAadhaarInfoByPhoneAPI

type DriverListAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'LIST
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
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'ACTIVITY
    :> Common.DriverActivityAPI

type EnableDriverAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'ENABLE
    :> Common.EnableDriverAPI

type DisableDriverAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'DISABLE
    :> Common.DisableDriverAPI

type BlockDriverWithReasonAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'BLOCK_WITH_REASON
    :> Common.BlockDriverWithReasonAPI

type BlockDriverAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'BLOCK
    :> Common.BlockDriverAPI

type BlockReasonListAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'BLOCK_REASON_LIST
    :> Common.DriverBlockReasonListAPI

type UnblockDriverAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'UNBLOCK
    :> Common.UnblockDriverAPI

type DriverLocationAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'LOCATION
    :> Common.DriverLocationAPI

type DriverInfoAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'INFO
    :> Common.DriverInfoAPI

type DeleteDriverAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'DELETE_DRIVER
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
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'DELETE_RC
    :> Common.DeleteRCAPI

type UnlinkDLAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'UNLINK_DL
    :> Common.UnlinkDLAPI

type UnlinkAadhaarAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'UNLINK_AADHAAR
    :> Common.UnlinkAadhaarAPI

type UpdatePhoneNumberAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'UPDATE_PHONE_NUMBER
    :> Common.UpdatePhoneNumberAPI

type UpdateDriverAadhaarAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'AADHAAR_UPDATE
    :> Common.UpdateDriverAadhaarAPI

type AddVehicleAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'ADD_VEHICLE
    :> Common.AddVehicleAPI

type AddVehicleForFleetAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'ADD_VEHICLE_FLEET
    :> Common.AddVehicleForFleetAPI

type GetAllVehicleForFleetAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'GET_ALL_VEHICLE_FOR_FLEET
    :> Common.GetAllVehicleForFleetAPI

type GetAllDriverForFleetAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'GET_ALL_DRIVERS_FOR_FLEET
    :> Common.GetAllDriverForFleetAPI

type FleetUnlinkVehicleAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'FLEET_UNLINK_VEHICLE
    :> Common.FleetUnlinkVehicleAPI

type FleetRemoveVehicleAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'FLEET_REMOVE_VEHICLE
    :> Common.FleetRemoveVehicleAPI

type FleetRemoveDriverAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'FLEET_REMOVE_DRIVER
    :> Common.FleetRemoveDriverAPI

type FleetTotalEarningAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'FLEET_TOTAL_EARNING
    :> Common.FleetTotalEarningAPI

type FleetVehicleEarningAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'FLEET_VEHICLE_EARNING
    :> Common.FleetVehicleEarningAPI

type FleetDriverEarningAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'FLEET_DRIVER_EARNING
    :> Common.FleetDriverEarningAPI

type UpdateDriverNameAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'UPDATE_DRIVER_NAME
    :> Common.UpdateDriverNameAPI

type ClearOnRideStuckDriversAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'CLEAR_ON_RIDE_STUCK_DRIVER_IDS
    :> Common.ClearOnRideStuckDriversAPI

type GetDriverHomeLocationAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'GET_DRIVER_HOME_LOCATION
    :> Common.GetDriverHomeLocationAPI

type UpdateDriverHomeLocationAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'UPDATE_DRIVER_HOME_LOCATION
    :> Common.UpdateDriverHomeLocationAPI

type IncrementDriverGoToCountAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'INCREMENT_DRIVER_GO_TO_COUNT
    :> Common.IncrementDriverGoToCountAPI

type GetDriverGoHomeInfoAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'GET_DRIVER_GO_HOME_INFO
    :> Common.GetDriverGoHomeInfoAPI

type DriverPaymentHistoryAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'PAYMENT_HISTORY
    :> ADSubscription.DriverPaymentHistoryAPI

type DriverPaymentHistoryEntityDetailsAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'PAYMENT_HISTORY_ENTITY_DETAILS
    :> ADSubscription.DriverPaymentHistoryEntityDetailsAPI

type DriverSubscriptionDriverFeeAndInvoiceUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'DRIVER_SUBSCRIPTION_DRIVER_FEE_AND_INVOICE_UPDATE
    :> Common.UpdateSubscriptionDriverFeeAndInvoiceAPI

type GetFleetDriverVehicleAssociationAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'GET_DRIVER_VEHICLE_ASSOCIATION
    :> Common.GetFleetDriverVehicleAssociationAPI

type GetFleetDriverAssociationAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'GET_DRIVER_ASSOCIATION
    :> Common.GetFleetDriverAssociationAPI

type GetFleetVehicleAssociationAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'GET_VEHICLE_ASSOCIATION
    :> Common.GetFleetVehicleAssociationAPI

type SetVehicleDriverRcStatusForFleetAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'SET_VEHICLE_DRIVER_RC_STATUS_FOR_FLEET
    :> Common.SetVehicleDriverRcStatusForFleetAPI

type SendMessageToDriverViaDashboardAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'SEND_DASHBOARD_MESSAGE
    :> ADSubscription.SendMessageToDriverViaDashboardAPI

type SendDummyRideRequestToDriverViaDashboardAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'SEND_DUMMY_NOTIFICATION
    :> Common.SendDummyRideRequestToDriverAPI

type ChangeOperatingCityAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'CHANGE_OPERATING_CITY
    :> Common.ChangeOperatingCityAPI

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  driverDocuments merchantId city
    :<|> driverAadhaarInfo merchantId city
    :<|> driverAadhaarInfoByPhone merchantId city
    :<|> listDriver merchantId city
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
    :<|> updateSubscriptionDriverFeeAndInvoice merchantId city
    :<|> getFleetDriverVehicleAssociation merchantId city
    :<|> getFleetDriverAssociation merchantId city
    :<|> getFleetVehicleAssociation merchantId city
    :<|> setVehicleDriverRcStatusForFleet merchantId city
    :<|> sendMessageToDriverViaDashboard merchantId city
    :<|> sendDummyRideRequestToDriverViaDashboard merchantId city
    :<|> changeOperatingCity merchantId city

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

buildManagementServerTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.DriverEndpoint ->
  ApiTokenInfo ->
  Id Common.Driver ->
  Maybe request ->
  m DT.Transaction
buildManagementServerTransaction endpoint apiTokenInfo driverId =
  T.buildTransaction (DT.DriverAPI endpoint) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) (Just driverId) Nothing

driverDocuments :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> FlowHandler Common.DriverDocumentsInfoRes
driverDocuments merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.driverDocumentsInfo)

driverAadhaarInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler Common.DriverAadhaarInfoRes
driverAadhaarInfo merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.driverAadhaarInfo) driverId

driverAadhaarInfoByPhone :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> FlowHandler Common.DriverAadhaarInfoByPhoneReq
driverAadhaarInfoByPhone merchantShortId opCity apiTokenInfo phoneNo = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.driverAadhaarInfoByPhone) phoneNo

listDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> FlowHandler Common.DriverListRes
listDriver merchantShortId opCity apiTokenInfo mbLimit mbOffset verified enabled blocked mbSubscribed phone mbVehicleNumberSearchString = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.listDrivers) mbLimit mbOffset verified enabled blocked mbSubscribed phone mbVehicleNumberSearchString

getDriverDue :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Text -> FlowHandler [Common.DriverOutstandingBalanceResp]
getDriverDue merchantShortId opCity apiTokenInfo mbMobileCountryCode phone = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPP checkedMerchantId opCity (.drivers.getDriverDue) mbMobileCountryCode phone

driverActivity :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> FlowHandler Common.DriverActivityRes
driverActivity merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.driverActivity)

collectCash :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
collectCash merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.CollectCashEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.drivers.collectCash) driverId apiTokenInfo.personId.getId

exemptCash :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
exemptCash merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.ExemptCashEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.drivers.exemptCash) driverId apiTokenInfo.personId.getId

enableDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
enableDriver merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.EnableDriverEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.drivers.enableDriver) driverId

disableDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
disableDriver merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction Common.DisableDriverEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.disableDriver) driverId

blockDriverWithReason :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.BlockDriverWithReasonReq -> FlowHandler APISuccess
blockDriverWithReason merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  person <- QP.findById apiTokenInfo.personId >>= fromMaybeM (PersonNotFound apiTokenInfo.personId.getId)
  let dashboardUserName = person.firstName <> " " <> person.lastName
  transaction <- buildManagementServerTransaction Common.BlockDriverWithReasonEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.blockDriverWithReason) driverId dashboardUserName req

blockDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
blockDriver merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction Common.BlockDriverEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.blockDriver) driverId

blockReasonList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> FlowHandler [Common.BlockReason]
blockReasonList merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.blockReasonList)

unblockDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
unblockDriver merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  person <- QP.findById apiTokenInfo.personId >>= fromMaybeM (PersonNotFound apiTokenInfo.personId.getId)
  let dashboardUserName = person.firstName <> " " <> person.lastName
  transaction <- buildManagementServerTransaction Common.UnblockDriverEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.unblockDriver) driverId dashboardUserName

driverLocation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation merchantShortId opCity apiTokenInfo mbLimit mbOffset req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.driverLocation) mbLimit mbOffset req

driverInfo ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  FlowHandler Common.DriverInfoRes
driverInfo merchantShortId opCity apiTokenInfo mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber mbRcNumber = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  unless (length (catMaybes [mbMobileNumber, mbVehicleNumber, mbDlNumber, mbRcNumber]) == 1) $
    throwError $ InvalidRequest "Exactly one of query parameters \"mobileNumber\", \"vehicleNumber\", \"dlNumber\", \"rcNumber\" is required"
  when (isJust mbMobileCountryCode && isNothing mbMobileNumber) $
    throwError $ InvalidRequest "\"mobileCountryCode\" can be used only with \"mobileNumber\""
  encPerson <- QP.findById apiTokenInfo.personId >>= fromMaybeM (PersonNotFound apiTokenInfo.personId.getId)
  role <- QRole.findById encPerson.roleId >>= fromMaybeM (RoleNotFound encPerson.roleId.getId)
  let mbFleet = role.dashboardAccessType == DRole.FLEET_OWNER
  Client.callDriverOfferBPP checkedMerchantId opCity (.drivers.driverInfo) mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber mbRcNumber apiTokenInfo.personId.getId mbFleet

deleteDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
deleteDriver merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction Common.DeleteDriverEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.deleteDriver) driverId

unlinkVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
unlinkVehicle merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.UnlinkVehicleEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.drivers.unlinkVehicle) driverId

updatePhoneNumber :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> FlowHandler APISuccess
updatePhoneNumber merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateUpdatePhoneNumberReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction Common.UpdatePhoneNumberEndpoint apiTokenInfo driverId $ Just req
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.updatePhoneNumber) driverId req

updateByPhoneNumber :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Common.UpdateDriverDataReq -> FlowHandler APISuccess
updateByPhoneNumber merchantShortId opCity apiTokenInfo phoneNo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.updateByPhoneNumber) phoneNo req

addVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.AddVehicleReq -> FlowHandler APISuccess
addVehicle merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateAddVehicleReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.AddVehicleEndpoint apiTokenInfo driverId $ Just req
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.drivers.addVehicle) driverId req

addVehicleForFleet :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Maybe Text -> Common.AddVehicleReq -> FlowHandler APISuccess
addVehicleForFleet merchantShortId opCity apiTokenInfo phoneNo mbMobileCountryCode req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateAddVehicleReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDynamicOfferDriverAppFleetApi checkedMerchantId opCity (.operations.addVehicleForFleet) phoneNo mbMobileCountryCode apiTokenInfo.personId.getId req

getAllVehicleForFleet :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> FlowHandler Common.ListVehicleRes
getAllVehicleForFleet merchantShortId opCity apiTokenInfo mbLimit mbOffset = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDynamicOfferDriverAppFleetApi checkedMerchantId opCity (.operations.getAllVehicleForFleet) apiTokenInfo.personId.getId mbLimit mbOffset

getAllDriverForFleet :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> FlowHandler Common.FleetListDriverRes
getAllDriverForFleet merchantShortId opCity apiTokenInfo mbLimit mbOffset = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDynamicOfferDriverAppFleetApi checkedMerchantId opCity (.operations.getAllDriverForFleet) apiTokenInfo.personId.getId mbLimit mbOffset

fleetUnlinkVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Text -> FlowHandler APISuccess
fleetUnlinkVehicle merchantShortId opCity apiTokenInfo driverId vehicleNo = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDynamicOfferDriverAppFleetApi checkedMerchantId opCity (.operations.fleetUnlinkVehicle) apiTokenInfo.personId.getId driverId vehicleNo

fleetRemoveVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> FlowHandler APISuccess
fleetRemoveVehicle merchantShortId opCity apiTokenInfo vehicleNo = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDynamicOfferDriverAppFleetApi checkedMerchantId opCity (.operations.fleetRemoveVehicle) apiTokenInfo.personId.getId vehicleNo

fleetRemoveDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
fleetRemoveDriver merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDynamicOfferDriverAppFleetApi checkedMerchantId opCity (.operations.fleetRemoveDriver) apiTokenInfo.personId.getId driverId

fleetTotalEarning :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> FlowHandler Common.FleetTotalEarningResponse
fleetTotalEarning merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDynamicOfferDriverAppFleetApi checkedMerchantId opCity (.operations.fleetTotalEarning) apiTokenInfo.personId.getId

fleetVehicleEarning :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Maybe (Id Common.Driver) -> FlowHandler Common.FleetEarningRes
fleetVehicleEarning merchantShortId opCity apiTokenInfo vehicleNo mbDriverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDynamicOfferDriverAppFleetApi checkedMerchantId opCity (.operations.fleetVehicleEarning) apiTokenInfo.personId.getId vehicleNo mbDriverId

fleetDriverEarning :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler Common.FleetEarningRes
fleetDriverEarning merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDynamicOfferDriverAppFleetApi checkedMerchantId opCity (.operations.fleetDriverEarning) apiTokenInfo.personId.getId driverId

updateDriverName :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UpdateDriverNameReq -> FlowHandler APISuccess
updateDriverName merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateUpdateDriverNameReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction Common.UpdateDriverNameEndpoint apiTokenInfo driverId $ Just req
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.updateDriverName) driverId req

unlinkDL :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
unlinkDL merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction Common.UnlinkDLEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.unlinkDL) driverId

unlinkAadhaar :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
unlinkAadhaar merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction Common.UnlinkAadhaarEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.unlinkAadhaar) driverId

endRCAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
endRCAssociation merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.EndRCAssociationEndpoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.drivers.endRCAssociation) driverId

setRCStatus :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.RCStatusReq -> FlowHandler APISuccess
setRCStatus merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.SetRCStatusEndpoint apiTokenInfo driverId $ Just req
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.drivers.setRCStatus) driverId req

deleteRC :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.DeleteRCReq -> FlowHandler APISuccess
deleteRC merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction Common.DeleteRCEndpoint apiTokenInfo driverId $ Just req
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.deleteRC) driverId req

clearOnRideStuckDrivers :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> FlowHandler Common.ClearOnRideStuckDriversRes
clearOnRideStuckDrivers merchantShortId opCity apiTokenInfo dbSyncTime = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.clearOnRideStuckDrivers) dbSyncTime

getDriverHomeLocation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler Common.GetHomeLocationsRes
getDriverHomeLocation merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.goHome.getDriverHomeLocation) driverId

updateDriverHomeLocation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UpdateDriverHomeLocationReq -> FlowHandler APISuccess
updateDriverHomeLocation merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction Common.UpdateDriverHomeLocationEndpoint apiTokenInfo driverId $ Just req
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.goHome.updateDriverHomeLocation) driverId req

incrementDriverGoToCount :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
incrementDriverGoToCount merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction Common.IncrementDriverGoToCountEndPoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.goHome.incrementDriverGoToCount) driverId

getDriverGoHomeInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler Common.CachedGoHomeRequestInfoRes
getDriverGoHomeInfo merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.goHome.getDriverGoHomeInfo) driverId

getPaymentHistory :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Maybe INV.InvoicePaymentMode -> Maybe Int -> Maybe Int -> FlowHandler Driver.HistoryEntityV2
getPaymentHistory merchantShortId opCity apiTokenInfo driverId paymentMode limit offset = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.subscription.getPaymentHistory) driverId paymentMode limit offset

getPaymentHistoryEntityDetails :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Id INV.Invoice -> FlowHandler Driver.HistoryEntryDetailsEntityV2
getPaymentHistoryEntityDetails merchantShortId opCity apiTokenInfo driverId invoiceId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.subscription.getPaymentHistoryEntityDetails) driverId invoiceId

updateSubscriptionDriverFeeAndInvoice :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.SubscriptionDriverFeesAndInvoicesToUpdate -> FlowHandler Common.SubscriptionDriverFeesAndInvoicesToUpdate
updateSubscriptionDriverFeeAndInvoice merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction Common.UpdateSubscriptionDriverFeeAndInvoiceEndpoint apiTokenInfo driverId $ Just req
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.subscription.updateSubscriptionDriverFeeAndInvoice) driverId req

getFleetDriverVehicleAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> FlowHandler Common.DrivertoVehicleAssociationRes
getFleetDriverVehicleAssociation merchantId opCity apiTokenInfo mbLimit mbOffset = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDynamicOfferDriverAppFleetApi checkedMerchantId opCity (.operations.getFleetDriverVehicleAssociation) apiTokenInfo.personId.getId mbLimit mbOffset

getFleetDriverAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> FlowHandler Common.DrivertoVehicleAssociationRes
getFleetDriverAssociation merhcantId opCity apiTokenInfo mbLimit mbOffset = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merhcantId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDynamicOfferDriverAppFleetApi checkedMerchantId opCity (.operations.getFleetDriverAssociation) apiTokenInfo.personId.getId mbLimit mbOffset

getFleetVehicleAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> FlowHandler Common.DrivertoVehicleAssociationRes
getFleetVehicleAssociation merhcantId opCity apiTokenInfo mbLimit mbOffset = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merhcantId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDynamicOfferDriverAppFleetApi checkedMerchantId opCity (.operations.getFleetVehicleAssociation) apiTokenInfo.personId.getId mbLimit mbOffset

setVehicleDriverRcStatusForFleet :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.RCStatusReq -> FlowHandler APISuccess
setVehicleDriverRcStatusForFleet merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction Common.SetVehicleDriverRcStatusForFleetEndpoint apiTokenInfo driverId $ Just req
  T.withTransactionStoring transaction $
    Client.callDynamicOfferDriverAppFleetApi checkedMerchantId opCity (.operations.setVehicleDriverRcStatusForFleet) driverId apiTokenInfo.personId.getId req

sendMessageToDriverViaDashboard :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> DDriver.SendSmsReq -> FlowHandler APISuccess
sendMessageToDriverViaDashboard merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction Common.SendMessageToDriverViaDashboardEndPoint apiTokenInfo driverId (Just $ DDriver.VolunteerTransactionStorageReq apiTokenInfo.personId.getId driverId.getId (show req.messageKey) (show req.channel) (show $ fromMaybe "" req.overlayKey) (show $ fromMaybe "" req.messageId))
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.subscription.sendMessageToDriverViaDashboard) driverId apiTokenInfo.personId.getId req

sendDummyRideRequestToDriverViaDashboard :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
sendDummyRideRequestToDriverViaDashboard merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildManagementServerTransaction Common.SendDummyRideRequestToDriverViaDashboardEndPoint apiTokenInfo driverId T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.sendDummyRideRequestToDriverViaDashboard) driverId

changeOperatingCity :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.ChangeOperatingCityReq -> FlowHandler APISuccess
changeOperatingCity merchantShortId opCity apiTokenInfo driverId req =
  withFlowHandlerAPI' $ do
    checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
    transaction <- buildTransaction Common.ChangeOperatingCityEndpoint apiTokenInfo driverId (Just req)
    T.withTransactionStoring transaction $
      Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverCommon.changeOperatingCity) driverId req
