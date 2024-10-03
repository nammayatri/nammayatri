{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Management.Driver
  ( getDriverDocumentsInfo,
    getDriverAadhaarInfo,
    getDriverAadhaarInfobyMobileNumber,
    getDriverList,
    getDriverActivity,
    postDriverDisable,
    postDriverPersonNumbers,
    postDriverPersonId,
    postDriverAcRestrictionUpdate,
    postDriverBlockWithReason,
    postDriverBlock,
    getDriverBlockReasonList,
    postDriverUnblock,
    getDriverLocation,
    deleteDriverPermanentlyDelete,
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
    getDriverDriverLicenseDetails,
    getDriverSearchRequests,
  )
where

import qualified API.Types.ProviderPlatform.Management.Driver as Common
import "dashboard-helper-api" Dashboard.Common.Driver (DriverLicenseD (..))
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Fleet.Driver as Common
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Action.Dashboard.Driver.Notification as DDN
import qualified Domain.Types.Common as DrInfo
import qualified Domain.Types.DriverLicense as DL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequestForDriver as SR
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.SystemConfigs ()
import qualified Storage.Clickhouse.SearchRequestForDriver as CSR
import qualified Storage.Queries.DriverLicense as QDL
import Tools.Error

-- TODO move handlers from Domain.Action.Dashboard.Driver
getDriverDocumentsInfo :: ShortId DM.Merchant -> Context.City -> Flow Common.DriverDocumentsInfoRes
getDriverDocumentsInfo = DDriver.driverDocumentsInfo

getDriverAadhaarInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow Common.DriverAadhaarInfoRes
getDriverAadhaarInfo = DDriver.driverAadhaarInfo

getDriverAadhaarInfobyMobileNumber :: ShortId DM.Merchant -> Context.City -> Text -> Flow Common.DriverAadhaarInfoByPhoneReq
getDriverAadhaarInfobyMobileNumber = DDriver.driverAadhaarInfoByPhone

getDriverList :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> Flow Common.DriverListRes
getDriverList = DDriver.listDrivers

getDriverActivity :: ShortId DM.Merchant -> Context.City -> Flow Common.DriverActivityRes
getDriverActivity = DDriver.driverActivity

postDriverDisable :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
postDriverDisable = DDriver.disableDriver

postDriverAcRestrictionUpdate :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateACUsageRestrictionReq -> Flow APISuccess
postDriverAcRestrictionUpdate = DDriver.updateACUsageRestriction

postDriverBlockWithReason :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Common.BlockDriverWithReasonReq -> Flow APISuccess
postDriverBlockWithReason = DDriver.blockDriverWithReason

postDriverBlock :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
postDriverBlock = DDriver.blockDriver

getDriverBlockReasonList :: ShortId DM.Merchant -> Context.City -> Flow [Common.BlockReason]
getDriverBlockReasonList _ _ = DDriver.blockReasonList

postDriverUnblock :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Flow APISuccess
postDriverUnblock = DDriver.unblockDriver

getDriverLocation :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Common.DriverIds -> Flow Common.DriverLocationRes
getDriverLocation = DDriver.driverLocation

deleteDriverPermanentlyDelete :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
deleteDriverPermanentlyDelete = DDriver.deleteDriver

postDriverUnlinkDL :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
postDriverUnlinkDL = DDriver.unlinkDL

postDriverUnlinkAadhaar :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
postDriverUnlinkAadhaar = DDriver.unlinkAadhaar

postDriverUpdatePhoneNumber :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> Flow APISuccess
postDriverUpdatePhoneNumber = DDriver.updatePhoneNumber

postDriverUpdateByPhoneNumber :: ShortId DM.Merchant -> Context.City -> Text -> Common.UpdateDriverDataReq -> Flow APISuccess
postDriverUpdateByPhoneNumber = DDriver.updateByPhoneNumber

postDriverUpdateName :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateDriverNameReq -> Flow APISuccess
postDriverUpdateName = DDriver.updateDriverName

postDriverDeleteRC :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.DeleteRCReq -> Flow APISuccess
postDriverDeleteRC = DDriver.deleteRC

getDriverClearStuckOnRide :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Flow Common.ClearOnRideStuckDriversRes
getDriverClearStuckOnRide = DDriver.clearOnRideStuckDrivers

postDriverSendDummyNotification :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
postDriverSendDummyNotification = DDN.sendDummyRideRequestToDriver

postDriverChangeOperatingCity :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.ChangeOperatingCityReq -> Flow APISuccess
postDriverChangeOperatingCity = DDriver.changeOperatingCity

getDriverGetOperatingCity :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Maybe Text -> Maybe (Id Common.Ride) -> Flow Common.GetOperatingCityResp
getDriverGetOperatingCity = DDriver.getOperatingCity

postDriverPauseOrResumeServiceCharges :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.PauseOrResumeServiceChargesReq -> Flow APISuccess
postDriverPauseOrResumeServiceCharges = DDriver.setServiceChargeEligibleFlagInDriverPlan

postDriverUpdateRCInvalidStatus :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateRCInvalidStatusReq -> Flow APISuccess
postDriverUpdateRCInvalidStatus merchantShortId opCity _ = DDriver.updateRCInvalidStatus merchantShortId opCity

postDriverUpdateVehicleVariant :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateVehicleVariantReq -> Flow APISuccess
postDriverUpdateVehicleVariant merchantShortId opCity _ = DDriver.updateVehicleVariant merchantShortId opCity

postDriverBulkReviewRCVariant :: ShortId DM.Merchant -> Context.City -> [Common.ReviewRCVariantReq] -> Flow [Common.ReviewRCVariantRes]
postDriverBulkReviewRCVariant = DDriver.bulkReviewRCVariant

postDriverUpdateDriverTag :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateDriverTagReq -> Flow APISuccess
postDriverUpdateDriverTag = DDriver.updateDriverTag

postDriverClearFee :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.ClearDriverFeeReq -> Flow APISuccess
postDriverClearFee = DDriver.postDriverClearFee

postDriverPersonNumbers :: ShortId DM.Merchant -> Context.City -> Common.PersonIdsReq -> Flow [Common.PersonRes]
postDriverPersonNumbers = DDriver.getDriverPersonNumbers

postDriverPersonId :: ShortId DM.Merchant -> Context.City -> Common.PersonMobileNoReq -> Flow [Common.PersonRes]
postDriverPersonId = DDriver.getDriverPersonId

getDriverPanAadharSelfieDetails :: ShortId DM.Merchant -> Context.City -> Text -> Text -> Flow Common.PanAadharSelfieDetailsResp
getDriverPanAadharSelfieDetails = DDriver.getDriverPanAadharSelfieDetails

postDriverSyncDocAadharPan :: ShortId DM.Merchant -> Context.City -> Common.AadharPanSyncReq -> Flow APISuccess
postDriverSyncDocAadharPan = DDriver.postDriverSyncDocAadharPan

postDriverUpdateVehicleManufacturing :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateVehicleManufacturingReq -> Flow APISuccess
postDriverUpdateVehicleManufacturing = DDriver.postDriverUpdateVehicleManufacturing

postDriverRefundByPayout :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.RefundByPayoutReq -> Flow APISuccess
postDriverRefundByPayout = DDriver.postDriverRefundByPayout

getDriverSecurityDepositStatus :: (ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Maybe Common.ServiceNames -> Flow [Common.SecurityDepositDfStatusRes])
getDriverSecurityDepositStatus = DDriver.getDriverSecurityDepositStatus

getDriverDriverLicenseDetails :: ShortId DM.Merchant -> Context.City -> Id Common.DriverLicense -> Flow DriverLicenseD
getDriverDriverLicenseDetails _ _ dlId = do
  mbDriverLicense <- B.runInReplica $ QDL.findByDLNumber (getId dlId)
  case mbDriverLicense of
    Just DL.DriverLicense {..} -> do
      licenseNo <- decrypt licenseNumber
      pure
        DriverLicenseD
          { documentImageId1 = cast documentImageId1,
            documentImageId2 = case documentImageId2 of
              Just docId -> Just $ cast docId
              Nothing -> Nothing,
            driverId = cast driverId,
            id = cast id,
            licenseNumber = licenseNo,
            merchantId = case merchantId of
              Just mId -> Just $ cast mId
              Nothing -> Nothing,
            ..
          }
    Nothing -> throwError (InvalidRequest "Driver license not found")

getDriverSearchRequests :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Int -> Flow [Common.SearchRequestForDriver]
getDriverSearchRequests _ _ dId xMin = do
  when (xMin > 180) $ throwError (InvalidRequest "xMin should be less than 180")
  searchReqs <- B.runInReplica $ CSR.findByDriverForLastXMinute (cast dId) xMin
  pure $ map buildSearchRequestForDriver searchReqs
  where
    buildSearchRequestForDriver :: (CSR.SearchRequestForDriverT Identity) -> Common.SearchRequestForDriver
    buildSearchRequestForDriver CSR.SearchRequestForDriverT {..} =
      Common.SearchRequestForDriver
        { id = cast id,
          driverId = cast dId,
          goHomeRequestId = case goHomeRequestId of
            Just ghId -> Just $ cast ghId
            Nothing -> Nothing,
          merchantId = case merchantId of
            Just mId -> Just $ cast mId
            Nothing -> Nothing,
          merchantOperatingCityId = cast merchantOperatingCityId,
          mode = Just $ castDriverStatus mode,
          notificationSource = Just $ castNotificationSource notificationSource,
          requestId = cast requestId,
          response = Just $ castSearchRequestForDriverResponse response,
          searchTryId = cast searchTryId,
          status = castDriverSearchRequestStatus status,
          createdAt = createdAt.getDateTime,
          ..
        }

castDriverStatus :: Maybe DrInfo.DriverMode -> Common.DriverMode
castDriverStatus = \case
  Just DrInfo.ONLINE -> Common.ONLINE
  Just DrInfo.OFFLINE -> Common.OFFLINE
  Just DrInfo.SILENT -> Common.SILENT
  Nothing -> Common.OFFLINE

castNotificationSource :: Maybe SR.NotificationSource -> Common.NotificationSource
castNotificationSource = \case
  Just SR.FCM -> Common.FCM
  Just SR.GRPC -> Common.GRPC
  Nothing -> Common.FCM

castSearchRequestForDriverResponse :: Maybe SR.SearchRequestForDriverResponse -> Common.SearchRequestForDriverResponse
castSearchRequestForDriverResponse = \case
  Just SR.Accept -> Common.Accept
  Just SR.Reject -> Common.Reject
  Just SR.Pulled -> Common.Pulled
  Nothing -> Common.Reject

castDriverSearchRequestStatus :: SR.DriverSearchRequestStatus -> Text
castDriverSearchRequestStatus = \case
  SR.Active -> "Active"
  SR.Inactive -> "Inactive"
