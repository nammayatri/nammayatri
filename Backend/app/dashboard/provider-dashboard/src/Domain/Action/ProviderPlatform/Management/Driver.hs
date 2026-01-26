{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.ProviderPlatform.Management.Driver
  ( getDriverDocumentsInfo,
    getDriverAadhaarInfo,
    getDriverAadhaarInfobyMobileNumber,
    getDriverList,
    getDriverActivity,
    postDriverDisable,
    postDriverAcRestrictionUpdate,
    postDriverBlockWithReason,
    postDriverBlock,
    getDriverBlockReasonList,
    postDriverUnblock,
    getDriverLocation,
    deleteDriverPermanentlyDelete,
    postDriverPersonNumbers,
    postDriverPersonId,
    postDriverUnlinkDL,
    postDriverUnlinkAadhaar,
    postDriverUpdatePhoneNumber,
    postDriverUpdateByPhoneNumber,
    postDriverUpdateName,
    postDriverDeleteRC,
    getDriverClearStuckOnRide,
    postDriverSendDummyNotification,
    postDriverChangeOperatingCity,
    getDriverGetOperatingCity,
    postDriverPauseOrResumeServiceCharges,
    postDriverUpdateRCInvalidStatus,
    postDriverUpdateVehicleVariant,
    postDriverBulkReviewRCVariant,
    postDriverUpdateDriverTag,
    postDriverClearFee,
    getDriverPanAadharSelfieDetails,
    postDriverSyncDocAadharPan,
    postDriverUpdateVehicleManufacturing,
    postDriverRefundByPayout,
    getDriverSecurityDepositStatus,
    postDriverDriverDataDecryption,
    getDriverPanAadharSelfieDetailsList,
    postDriverBulkSubscriptionServiceUpdate,
    getDriverStats,
    getDriverEarnings,
    postDriverUpdateTagBulk,
    postDriverUpdateMerchant,
    postDriverUpdateRCInvalidStatusByRCNumber,
  )
where

import qualified API.Client.ProviderPlatform.Management as Client
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Driver as Common
import Data.Time.Calendar (Day)
import qualified "lib-dashboard" Domain.Types.Merchant as DM
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
import "lib-dashboard" Storage.Queries.MerchantAccess as QMerchantAccess
import "lib-dashboard" Storage.Queries.Person as QP
import "lib-dashboard" Storage.Queries.RegistrationToken as QRegistrationToken
import Tools.Auth.Api
import Tools.Auth.Merchant

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  ApiTokenInfo ->
  Maybe (Id Common.Driver) ->
  Maybe request ->
  m DT.Transaction
buildTransaction apiTokenInfo driverId =
  T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) driverId Nothing

getDriverDocumentsInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Flow Common.DriverDocumentsInfoRes
getDriverDocumentsInfo merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverDSL.getDriverDocumentsInfo)

getDriverAadhaarInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow Common.DriverAadhaarInfoRes
getDriverAadhaarInfo merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverDSL.getDriverAadhaarInfo) driverId

getDriverAadhaarInfobyMobileNumber :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Flow Common.DriverAadhaarInfoByPhoneReq
getDriverAadhaarInfobyMobileNumber merchantShortId opCity apiTokenInfo phoneNo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverDSL.getDriverAadhaarInfobyMobileNumber) phoneNo

getDriverList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Flow Common.DriverListRes
getDriverList merchantShortId opCity apiTokenInfo mbLimit mbOffset verified enabled blocked mbSubscribed phone mbVehicleNumberSearchString mbNameSearchString = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverDSL.getDriverList) mbLimit mbOffset verified enabled blocked mbSubscribed phone mbVehicleNumberSearchString mbNameSearchString

getDriverActivity :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Flow Common.DriverActivityRes
getDriverActivity merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverDSL.getDriverActivity)

postDriverDisable :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverDisable merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverDisable) driverId

postDriverAcRestrictionUpdate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UpdateACUsageRestrictionReq -> Flow APISuccess
postDriverAcRestrictionUpdate merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverAcRestrictionUpdate) driverId req

postDriverBlockWithReason :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.BlockDriverWithReasonReq -> Flow APISuccess
postDriverBlockWithReason merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  person <- QP.findById apiTokenInfo.personId >>= fromMaybeM (PersonNotFound apiTokenInfo.personId.getId)
  let dashboardUserName = person.firstName <> " " <> person.lastName
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverBlockWithReason) driverId dashboardUserName req

postDriverBlock :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverBlock merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverBlock) driverId

getDriverBlockReasonList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Flow [Common.BlockReason]
getDriverBlockReasonList merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverDSL.getDriverBlockReasonList)

postDriverUnblock :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Maybe UTCTime -> Maybe UTCTime -> Flow APISuccess
postDriverUnblock merchantShortId opCity apiTokenInfo driverId preventWeeklyCancellationRateBlockingTill preventDailyCancellationRateBlockingTill = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  person <- QP.findById apiTokenInfo.personId >>= fromMaybeM (PersonNotFound apiTokenInfo.personId.getId)
  let dashboardUserName = person.firstName <> " " <> person.lastName
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverUnblock) driverId dashboardUserName preventWeeklyCancellationRateBlockingTill preventDailyCancellationRateBlockingTill

getDriverLocation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Common.DriverIds -> Flow Common.DriverLocationRes
getDriverLocation merchantShortId opCity apiTokenInfo mbLimit mbOffset req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverDSL.getDriverLocation) mbLimit mbOffset req

deleteDriverPermanentlyDelete :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
deleteDriverPermanentlyDelete merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $ do
    result <- Client.callManagementAPI checkedMerchantId opCity (.driverDSL.deleteDriverPermanentlyDelete) driverId
    let personId = Kernel.Types.Id.cast driverId
    mbPerson <- QP.findById personId
    whenJust mbPerson $ \_ -> do
      QMerchantAccess.deleteAllByPersonId personId
      QRegistrationToken.deleteAllByPersonId personId
      QP.deletePerson personId
      logTagInfo "PermanentlyDelete - successfully removed person and associated data for personId: " (show personId)
    pure result

postDriverUnlinkDL :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverUnlinkDL merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverUnlinkDL) driverId

postDriverUnlinkAadhaar :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverUnlinkAadhaar merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverUnlinkAadhaar) driverId

postDriverUpdatePhoneNumber :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> Flow APISuccess
postDriverUpdatePhoneNumber merchantShortId opCity apiTokenInfo driverId req = do
  runRequestValidation Common.validateUpdatePhoneNumberReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) $ Just req
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverUpdatePhoneNumber) driverId req

--  UpdateDriverAadhaarAPI
postDriverUpdateByPhoneNumber :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Common.UpdateDriverDataReq -> Flow APISuccess
postDriverUpdateByPhoneNumber merchantShortId opCity apiTokenInfo phoneNo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverUpdateByPhoneNumber) phoneNo req

postDriverUpdateName :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UpdateDriverNameReq -> Flow APISuccess
postDriverUpdateName merchantShortId opCity apiTokenInfo driverId req = do
  runRequestValidation Common.validateUpdateDriverNameReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) $ Just req
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverUpdateName) driverId req

postDriverDeleteRC :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.DeleteRCReq -> Flow APISuccess
postDriverDeleteRC merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) $ Just req
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverDeleteRC) driverId req

getDriverClearStuckOnRide :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Flow Common.ClearOnRideStuckDriversRes
getDriverClearStuckOnRide merchantShortId opCity apiTokenInfo dbSyncTime = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverDSL.getDriverClearStuckOnRide) dbSyncTime

postDriverSendDummyNotification :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverSendDummyNotification merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverSendDummyNotification) driverId

postDriverChangeOperatingCity :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.ChangeOperatingCityReq -> Flow APISuccess
postDriverChangeOperatingCity merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverChangeOperatingCity) driverId req

getDriverGetOperatingCity :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Maybe Text -> Maybe (Id Common.Ride) -> Flow Common.GetOperatingCityResp
getDriverGetOperatingCity merchantShortId opCity apiTokenInfo mbMobileCountryCode mbMobileNumber mbRideId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverDSL.getDriverGetOperatingCity) mbMobileCountryCode mbMobileNumber mbRideId

-- setServiceChargeEligibleFlagInDriverPlan
postDriverPauseOrResumeServiceCharges :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.PauseOrResumeServiceChargesReq -> Flow APISuccess
postDriverPauseOrResumeServiceCharges merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverPauseOrResumeServiceCharges) driverId req

postDriverUpdateRCInvalidStatus :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UpdateRCInvalidStatusReq -> Flow APISuccess
postDriverUpdateRCInvalidStatus merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverUpdateRCInvalidStatus) driverId req

postDriverUpdateVehicleVariant :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UpdateVehicleVariantReq -> Flow APISuccess
postDriverUpdateVehicleVariant merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverUpdateVehicleVariant) driverId req

postDriverBulkReviewRCVariant :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> [Common.ReviewRCVariantReq] -> Flow [Common.ReviewRCVariantRes]
postDriverBulkReviewRCVariant merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverBulkReviewRCVariant) req

postDriverUpdateDriverTag :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UpdateDriverTagReq -> Flow APISuccess
postDriverUpdateDriverTag merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverUpdateDriverTag) driverId req

postDriverClearFee :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.ClearDriverFeeReq -> Environment.Flow APISuccess
postDriverClearFee merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverClearFee) driverId req

postDriverPersonNumbers :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.PersonIdsReq -> Environment.Flow [Common.PersonRes]
postDriverPersonNumbers merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $ (do Client.callManagementAPI checkedMerchantId opCity (Common.addMultipartBoundary "XXX00XXX" . (.driverDSL.postDriverPersonNumbers)) req)

getDriverPanAadharSelfieDetails :: (ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Text -> Environment.Flow Common.PanAadharSelfieDetailsResp)
getDriverPanAadharSelfieDetails merchantShortId opCity apiTokenInfo countryCode phoneNo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverDSL.getDriverPanAadharSelfieDetails) countryCode phoneNo

postDriverSyncDocAadharPan :: (ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.AadharPanSyncReq -> Environment.Flow APISuccess)
postDriverSyncDocAadharPan merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverSyncDocAadharPan) req

postDriverPersonId :: (ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.PersonMobileNoReq -> Environment.Flow [Common.PersonRes])
postDriverPersonId merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $ do Client.callManagementAPI checkedMerchantId opCity (Common.addMultipartBoundary "XXX00XXX" . (.driverDSL.postDriverPersonId)) req

postDriverUpdateVehicleManufacturing :: (ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UpdateVehicleManufacturingReq -> Environment.Flow APISuccess)
postDriverUpdateVehicleManufacturing merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $ do Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverUpdateVehicleManufacturing) driverId req

postDriverRefundByPayout :: (ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.RefundByPayoutReq -> Environment.Flow APISuccess)
postDriverRefundByPayout merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction (do Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverRefundByPayout) driverId req)

getDriverSecurityDepositStatus :: (ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Maybe Common.ServiceNames -> Environment.Flow [Common.SecurityDepositDfStatusRes])
getDriverSecurityDepositStatus merchantShortId opCity apiTokenInfo driverId mbServiceName = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverDSL.getDriverSecurityDepositStatus) driverId mbServiceName

postDriverDriverDataDecryption :: (ShortId DM.Merchant -> City.City -> ApiTokenInfo -> [Common.DriverEncDataReq] -> Environment.Flow [Common.DriverDecDataResp])
postDriverDriverDataDecryption merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing (Just req)
  T.withTransactionStoring transaction (do Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverDriverDataDecryption) req)

getDriverPanAadharSelfieDetailsList :: (ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Id Common.Driver -> Environment.Flow [Common.PanAadharSelfieDetailsListResp])
getDriverPanAadharSelfieDetailsList merchantShortId opCity apiTokenInfo docType driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverDSL.getDriverPanAadharSelfieDetailsList) docType driverId

postDriverBulkSubscriptionServiceUpdate :: (ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.BulkServiceUpdateReq -> Environment.Flow APISuccess)
postDriverBulkSubscriptionServiceUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing (Just req)
  T.withTransactionStoring transaction $ (do Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverBulkSubscriptionServiceUpdate) req)

getDriverStats :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe (Id Common.Driver) -> Maybe Day -> Maybe Day -> Flow Common.DriverStatsRes
getDriverStats merchantShortId opCity apiTokenInfo entityId fromDate toDate = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverDSL.getDriverStats) entityId fromDate toDate apiTokenInfo.personId.getId

getDriverEarnings :: (ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Day -> Day -> Common.EarningType -> Id Common.Driver -> Environment.Flow Common.EarningPeriodStatsRes)
getDriverEarnings merchantShortId opCity apiTokenInfo from to earningType entityId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverDSL.getDriverEarnings) from to earningType entityId apiTokenInfo.personId.getId

postDriverUpdateTagBulk :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.UpdateTagBulkReq -> Environment.Flow [Common.UpdateTagBulkRes]
postDriverUpdateTagBulk merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (Common.addMultipartBoundary "XXX00XXX" . (.driverDSL.postDriverUpdateTagBulk)) req

postDriverUpdateMerchant :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UpdateDriverMerchantReq -> Environment.Flow APISuccess
postDriverUpdateMerchant merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverUpdateMerchant) driverId req

postDriverUpdateRCInvalidStatusByRCNumber :: (ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.UpdateRCInvalidStatusByRCNumberReq -> Environment.Flow APISuccess)
postDriverUpdateRCInvalidStatusByRCNumber merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $ (do Client.callManagementAPI checkedMerchantId opCity (.driverDSL.postDriverUpdateRCInvalidStatusByRCNumber) req)
