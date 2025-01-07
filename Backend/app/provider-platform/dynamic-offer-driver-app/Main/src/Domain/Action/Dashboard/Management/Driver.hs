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
    postDriverDriverDataDecryption,
    getDriverPanAadharSelfieDetailsList,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Driver as Common
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Action.Dashboard.Driver.Notification as DDN
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Storage.Beam.SystemConfigs ()

-- TODO move handlers from Domain.Action.Dashboard.Driver
getDriverDocumentsInfo :: ShortId DM.Merchant -> Context.City -> Flow Common.DriverDocumentsInfoRes
getDriverDocumentsInfo = DDriver.driverDocumentsInfo

getDriverAadhaarInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow Common.DriverAadhaarInfoRes
getDriverAadhaarInfo = DDriver.driverAadhaarInfo

getDriverAadhaarInfobyMobileNumber :: ShortId DM.Merchant -> Context.City -> Text -> Flow Common.DriverAadhaarInfoByPhoneReq
getDriverAadhaarInfobyMobileNumber = DDriver.driverAadhaarInfoByPhone

getDriverList :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Flow Common.DriverListRes
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

postDriverUnblock :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Maybe UTCTime -> Maybe UTCTime -> Flow APISuccess
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

postDriverDriverDataDecryption :: (ShortId DM.Merchant -> Context.City -> [Common.DriverEncDataReq] -> Flow [Common.DriverDecDataResp])
postDriverDriverDataDecryption = DDriver.postDriverDriverDataDecryption

getDriverPanAadharSelfieDetailsList :: (ShortId DM.Merchant -> Context.City -> Text -> Id Common.Driver -> Flow [Common.PanAadharSelfieDetailsListResp])
getDriverPanAadharSelfieDetailsList = DDriver.getDriverPanAadharSelfieDetailsList
