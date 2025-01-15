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

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.Driver as Common
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Action.Dashboard.Driver.Notification as DDN
import qualified Domain.Action.UI.Plan as DTPlan
import qualified Domain.Types.DriverInformation as DrInfo
import qualified Domain.Types.DriverPlan as DDPlan
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantMessage (MediaChannel (..), MessageKey (..))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import qualified Domain.Types.VehicleCategory as DVC
import Environment
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.EventTracking as SEVT
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.SystemConfigs ()
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.PlanExtra as CQP
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverPlan as QDP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
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
postDriverPauseOrResumeServiceCharges merchantShortId opCity driverId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  let serviceName = DDriver.mapServiceName req.serviceName
  driverPlan <- QDP.findByDriverIdWithServiceName personId serviceName
  mVehicle <- QVehicle.findById personId
  let vehicleCategory = mVehicle >>= (.category)
  case (driverPlan, req.planId) of
    (Just dp, Just planId) -> do
      if dp.planId == Id planId
        then do
          void $ toggleDriverSubscriptionByService (driver.id, driver.merchantId, driver.merchantOperatingCityId) serviceName (Id <$> req.planId) req.serviceChargeEligibility req.vehicleId vehicleCategory
        else do
          void $ DTPlan.planSwitch serviceName (Id planId) (driver.id, driver.merchantId, driver.merchantOperatingCityId)
          void $ toggleDriverSubscriptionByService (driver.id, driver.merchantId, driver.merchantOperatingCityId) serviceName (Id <$> req.planId) req.serviceChargeEligibility req.vehicleId vehicleCategory
    (Nothing, Just _) -> do
      void $ toggleDriverSubscriptionByService (driver.id, driver.merchantId, driver.merchantOperatingCityId) serviceName (Id <$> req.planId) req.serviceChargeEligibility req.vehicleId vehicleCategory
    (Just dp, Nothing) -> do
      transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      let enableServiceUsageCharge = dp.enableServiceUsageCharge
      when (enableServiceUsageCharge /= req.serviceChargeEligibility) $ do
        QDP.updateEnableServiceUsageChargeByDriverIdAndServiceName req.serviceChargeEligibility personId serviceName
        fork "track service toggle" $ do
          SEVT.trackServiceUsageChargeToggle dp (show <$> req.reason)
        when (serviceName == YATRI_RENTAL) $ do
          fork "notify rental event" $ do
            DDriver.notifyYatriRentalEventsToDriver req.vehicleId (getMkeyForEvent req.serviceChargeEligibility) personId transporterConfig (show <$> req.reason) WHATSAPP
    (Nothing, Nothing) -> throwError $ InvalidRequest "pls provide a plan Id to enable subscription"
  pure Success
  where
    getMkeyForEvent serviceChargeEligiblity = if serviceChargeEligiblity then YATRI_RENTAL_RESUME else YATRI_RENTAL_PAUSE

toggleDriverSubscriptionByService ::
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ServiceNames ->
  Maybe (Id Plan) ->
  Bool ->
  Maybe Text ->
  Maybe DVC.VehicleCategory ->
  Flow ()
toggleDriverSubscriptionByService (driverId, mId, mOpCityId) serviceName mbPlanToAssign toToggle mbVehicleNo mbVehicleCategory = do
  (autoPayStatus, driverPlan) <- DTPlan.getSubcriptionStatusWithPlan serviceName driverId
  transporterConfig <- CTC.findByMerchantOpCityId mOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound mOpCityId.getId)
  if toToggle
    then do
      planToAssign <- getPlanId mbPlanToAssign
      case autoPayStatus of
        Just DrInfo.ACTIVE -> pure ()
        _ -> callSubscribeFlowForDriver planToAssign
      whenJust mbVehicleNo $ \vehicleNo -> do
        QDP.updatesubscriptionServiceRelatedDataInDriverPlan driverId (DDPlan.RentedVehicleNumber vehicleNo) serviceName
      QDP.updateEnableServiceUsageChargeByDriverIdAndServiceName toToggle driverId serviceName
      fork "notify rental event" $ do
        DDriver.notifyYatriRentalEventsToDriver mbVehicleNo WHATSAPP_VEHICLE_LINKED_MESSAGE driverId transporterConfig Nothing WHATSAPP
    else do
      when (isJust driverPlan) $ do
        QDP.updateEnableServiceUsageChargeByDriverIdAndServiceName toToggle driverId serviceName
        fork "track service toggle" $ do
          case driverPlan of
            Just dp -> SEVT.trackServiceUsageChargeToggle dp Nothing
            Nothing -> pure ()
        fork "notify rental event" $ do
          DDriver.notifyYatriRentalEventsToDriver mbVehicleNo WHATSAPP_VEHICLE_UNLINKED_MESSAGE driverId transporterConfig Nothing WHATSAPP
  where
    getPlanId :: Maybe (Id Plan) -> Flow (Id Plan)
    getPlanId mbPlanId = do
      case mbPlanId of
        Nothing -> do
          plans <- maybe (pure []) (\vc -> CQP.findByMerchantOpCityIdAndTypeWithServiceName mOpCityId DEFAULT serviceName vc False) mbVehicleCategory
          case plans of
            [] -> throwError $ InternalError "No default plans found"
            [pl] -> pure pl.id
            _ -> throwError $ InternalError "Multiple default plans found"
        Just planId -> pure planId
    callSubscribeFlowForDriver :: Id Plan -> Flow ()
    callSubscribeFlowForDriver planId = do
      driverInfo' <- QDriverInfo.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
      let serviceSpecificData = maybe DDPlan.NoData DDPlan.RentedVehicleNumber mbVehicleNo
      _ <- DTPlan.planSubscribe serviceName planId (True, Just WHATSAPP) (cast driverId, mId, mOpCityId) driverInfo' serviceSpecificData
      pure ()

---------------------------------------------------------------------
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
