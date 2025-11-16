{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.ProviderPlatform.RideBooking.Driver
  ( getDriverPaymentDue,
    postDriverEnable,
    postDriverCollectCash,
    postDriverV2CollectCash,
    postDriverExemptCash,
    postDriverV2ExemptCash,
    getDriverInfo,
    getDriverFeedbackList,
    postDriverUnlinkVehicle,
    postDriverEndRCAssociation,
    postDriverAddVehicle,
    postDriverSetRCStatus,
    postDriverExemptDriverFee,
  )
where

import qualified API.Client.ProviderPlatform.RideBooking as Client
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.Driver as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as CommonFleet
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Role as DRole
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Person as QP
import "lib-dashboard" Storage.Queries.Role as QRole
import Tools.Auth.Api
import Tools.Auth.Merchant
import "lib-dashboard" Tools.Error

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  ApiTokenInfo ->
  Maybe (Id Common.Driver) ->
  Maybe request ->
  m DT.Transaction
buildTransaction apiTokenInfo driverId =
  T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP) (Just apiTokenInfo) driverId Nothing

getDriverPaymentDue :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Text -> Flow [Common.DriverOutstandingBalanceResp]
getDriverPaymentDue merchantShortId opCity apiTokenInfo mbMobileCountryCode phone = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRideBookingAPI checkedMerchantId opCity (.driverDSL.getDriverPaymentDue) mbMobileCountryCode phone

postDriverEnable :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverEnable merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRideBookingAPI checkedMerchantId opCity (.driverDSL.postDriverEnable) driverId

postDriverCollectCash :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverCollectCash merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRideBookingAPI checkedMerchantId opCity (.driverDSL.postDriverCollectCash) driverId apiTokenInfo.personId.getId

postDriverV2CollectCash :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.ServiceNames -> Flow APISuccess
postDriverV2CollectCash merchantShortId opCity apiTokenInfo driverId serviceName = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRideBookingAPI checkedMerchantId opCity (.driverDSL.postDriverV2CollectCash) driverId apiTokenInfo.personId.getId serviceName

postDriverExemptCash :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverExemptCash merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRideBookingAPI checkedMerchantId opCity (.driverDSL.postDriverExemptCash) driverId apiTokenInfo.personId.getId

postDriverV2ExemptCash :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.ServiceNames -> Flow APISuccess
postDriverV2ExemptCash merchantShortId opCity apiTokenInfo driverId serviceName = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRideBookingAPI checkedMerchantId opCity (.driverDSL.postDriverV2ExemptCash) driverId apiTokenInfo.personId.getId serviceName

getDriverInfo ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe (Id Common.Driver) ->
  Flow Common.DriverInfoRes
getDriverInfo merchantShortId opCity apiTokenInfo mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber mbRcNumber mbEmail mbPersonId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  unless (length (catMaybes [mbMobileNumber, mbVehicleNumber, mbDlNumber, mbRcNumber, mbEmail, getId <$> mbPersonId]) == 1) $
    throwError $ InvalidRequest "Exactly one of query parameters \"mobileNumber\", \"vehicleNumber\", \"dlNumber\", \"rcNumber\", \"email\" is required"
  when (isJust mbMobileCountryCode && isNothing mbMobileNumber) $
    throwError $ InvalidRequest "\"mobileCountryCode\" can be used only with \"mobileNumber\""
  encPerson <- QP.findById apiTokenInfo.personId >>= fromMaybeM (PersonNotFound apiTokenInfo.personId.getId)
  role <- QRole.findById encPerson.roleId >>= fromMaybeM (RoleNotFound encPerson.roleId.getId)
  let mbFleet = role.dashboardAccessType == DRole.FLEET_OWNER || role.dashboardAccessType == DRole.RENTAL_FLEET_OWNER
  Client.callRideBookingAPI checkedMerchantId opCity (.driverDSL.getDriverInfo) apiTokenInfo.personId.getId mbFleet mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber mbRcNumber mbEmail mbPersonId

postDriverUnlinkVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverUnlinkVehicle merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRideBookingAPI checkedMerchantId opCity (.driverDSL.postDriverUnlinkVehicle) driverId

postDriverEndRCAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverEndRCAssociation merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRideBookingAPI checkedMerchantId opCity (.driverDSL.postDriverEndRCAssociation) driverId

postDriverAddVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> CommonFleet.AddVehicleReq -> Flow APISuccess
postDriverAddVehicle merchantShortId opCity apiTokenInfo driverId req = do
  runRequestValidation CommonFleet.validateAddVehicleReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) $ Just req
  T.withTransactionStoring transaction $
    Client.callRideBookingAPI checkedMerchantId opCity (.driverDSL.postDriverAddVehicle) driverId req

postDriverSetRCStatus :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> CommonFleet.RCStatusReq -> Flow APISuccess
postDriverSetRCStatus merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) $ Just req
  T.withTransactionStoring transaction $
    Client.callRideBookingAPI checkedMerchantId opCity (.driverDSL.postDriverSetRCStatus) driverId req

postDriverExemptDriverFee :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.ServiceNames -> Common.ExemptionAndCashCollectionDriverFeeReq -> Flow APISuccess
postDriverExemptDriverFee merchantShortId opCity apiTokenInfo driverId serviceName req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) $ Just req
  T.withTransactionStoring transaction $
    Client.callRideBookingAPI checkedMerchantId opCity (.driverDSL.postDriverExemptDriverFee) driverId apiTokenInfo.personId.getId serviceName req

getDriverFeedbackList :: (ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe (Id Common.Driver) -> Maybe Text -> Maybe Text -> Flow Common.GetFeedbackListRes)
getDriverFeedbackList merchantShortId opCity apiTokenInfo personId mobileNumber mobileCountryCode = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRideBookingAPI checkedMerchantId opCity (.driverDSL.getDriverFeedbackList) personId mobileNumber mobileCountryCode
