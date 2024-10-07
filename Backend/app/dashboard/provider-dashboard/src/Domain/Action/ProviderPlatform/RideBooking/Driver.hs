{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.ProviderPlatform.RideBooking.Driver
  ( getDriverPaymentDue,
    postDriverEnable,
    postDriverCollectCash,
    postDriverV2CollectCash,
    postDriverExemptCash,
    postDriverV2ExemptCash,
    getDriverInfo,
    postDriverUnlinkVehicle,
    postDriverEndRCAssociation,
    postDriverAddVehicle,
    postDriverSetRCStatus,
    postDriverExemptDriverFee,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.RideBooking
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.RideBooking as Common
import qualified API.Types.ProviderPlatform.RideBooking.Driver
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.RideBooking.Driver as Common
import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Dashboard.ProviderPlatform.Fleet.Driver
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Fleet.Driver as CommonFleet
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.RideBooking.Driver as Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Role as DRole
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.City as City
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import qualified ProviderPlatformClient.DynamicOfferDriver.RideBooking
import qualified ProviderPlatformClient.DynamicOfferDriver.RideBooking as Client
import qualified SharedLogic.Transaction
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
  Common.DriverEndpointDSL ->
  ApiTokenInfo ->
  Maybe (Id Common.Driver) ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo driverId =
  T.buildTransaction (DT.ProviderRideBookingAPI $ Common.DriverAPI endpoint) (Just DRIVER_OFFER_BPP) (Just apiTokenInfo) driverId Nothing

getDriverPaymentDue :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Text -> Flow [Common.DriverOutstandingBalanceResp]
getDriverPaymentDue merchantShortId opCity apiTokenInfo mbMobileCountryCode phone = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPP checkedMerchantId opCity (.driverDSL.getDriverPaymentDue) mbMobileCountryCode phone

postDriverEnable :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverEnable merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.PostDriverEnableEndpoint apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.driverDSL.postDriverEnable) driverId

postDriverCollectCash :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverCollectCash merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.PostDriverCollectCashEndpoint apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.driverDSL.postDriverCollectCash) driverId apiTokenInfo.personId.getId

postDriverV2CollectCash :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.ServiceNames -> Flow APISuccess
postDriverV2CollectCash merchantShortId opCity apiTokenInfo driverId serviceName = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.PostDriverV2CollectCashEndpoint apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.driverDSL.postDriverV2CollectCash) driverId apiTokenInfo.personId.getId serviceName

postDriverExemptCash :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverExemptCash merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.PostDriverExemptCashEndpoint apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.driverDSL.postDriverExemptCash) driverId apiTokenInfo.personId.getId

postDriverV2ExemptCash :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.ServiceNames -> Flow APISuccess
postDriverV2ExemptCash merchantShortId opCity apiTokenInfo driverId serviceName = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.PostDriverV2ExemptCashEndpoint apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.driverDSL.postDriverV2ExemptCash) driverId apiTokenInfo.personId.getId serviceName

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
  Client.callDriverOfferBPP checkedMerchantId opCity (.driverDSL.getDriverInfo) apiTokenInfo.personId.getId mbFleet mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber mbRcNumber mbEmail mbPersonId

postDriverUnlinkVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverUnlinkVehicle merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.PostDriverUnlinkVehicleEndpoint apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.driverDSL.postDriverUnlinkVehicle) driverId

postDriverEndRCAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverEndRCAssociation merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.PostDriverEndRCAssociationEndpoint apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.driverDSL.postDriverEndRCAssociation) driverId

postDriverAddVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> CommonFleet.AddVehicleReq -> Flow APISuccess
postDriverAddVehicle merchantShortId opCity apiTokenInfo driverId req = do
  runRequestValidation CommonFleet.validateAddVehicleReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.PostDriverAddVehicleEndpoint apiTokenInfo (Just driverId) $ Just req
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.driverDSL.postDriverAddVehicle) driverId req

postDriverSetRCStatus :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> CommonFleet.RCStatusReq -> Flow APISuccess
postDriverSetRCStatus merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.PostDriverSetRCStatusEndpoint apiTokenInfo (Just driverId) $ Just req
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.driverDSL.postDriverSetRCStatus) driverId req

postDriverExemptDriverFee :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.ServiceNames -> Common.ExemptionAndCashCollectionDriverFeeReq -> Flow APISuccess
postDriverExemptDriverFee merchantShortId opCity apiTokenInfo driverId serviceName req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.PostDriverExemptDriverFeeEndpoint apiTokenInfo (Just driverId) $ Just req
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.driverDSL.postDriverExemptDriverFee) driverId apiTokenInfo.personId.getId serviceName req
