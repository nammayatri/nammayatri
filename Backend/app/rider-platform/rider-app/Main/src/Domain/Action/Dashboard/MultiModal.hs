{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.MultiModal (postMultiModalMultimodalFrfsDataPreprocess, postMultiModalMultimodalFrfsDataStatus, postMultiModalMultimodalFrfsDataVersionIsReady, postMultiModalMultimodalFrfsDataVersionApply) where

import qualified API.Types.RiderPlatform.Management.MultiModal
import Domain.Types.Extra.Rollout
import qualified Domain.Types.Merchant
-- import qualified Domain.Types.Stage as DTS
-- import qualified Storage.Queries.Stage as QS
import qualified Domain.Types.Rollout
import qualified Domain.Types.Version
import Domain.Types.VersionStageMapping
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Rollout as QR
import qualified Storage.Queries.Version as QV
import qualified Storage.Queries.VersionStageMapping as QVSM
import Tools.Error

postMultiModalMultimodalFrfsDataPreprocess :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.MultiModal.PreprocessFRFSDataReq -> Environment.Flow API.Types.RiderPlatform.Management.MultiModal.PreprocessFRFSDataResp)
postMultiModalMultimodalFrfsDataPreprocess _merchantShortId _opCity _req = do error "Logic yet to be decided"

-- Get All Stages for Data Type and City
-- Iterate on each stage, till reching the end, and create next stage if current is completed
-- PREPROCESSING -> Fail : Flag Failure in seperate `anomaly` column in the CUSTOM CSVs and upload of S3
--               -> Success: Create GTFS and move to next stage.
-- VALIDATION    -> Fail : Flag Failure in seperate `anomaly` column in the GTFS CSVs and upload of S3
--               -> Success: move to next stage.
-- UPLOAD        -> Fail : Flag Failure in Failure Reason.
--               -> Success: Done.

postMultiModalMultimodalFrfsDataStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.MultiModal.FRFSDataStatusReq -> Environment.Flow API.Types.RiderPlatform.Management.MultiModal.FRFSDataStatusResp)
postMultiModalMultimodalFrfsDataStatus _ _ req = do
  versionStageMappings <- QVSM.findAllByVersionId req.versionId
  stageData <- traverse mkFRFSDataStatus versionStageMappings
  return $ API.Types.RiderPlatform.Management.MultiModal.FRFSDataStatusResp req.versionId stageData

mkFRFSDataStatus :: (MonadFlow m) => VersionStageMapping -> m API.Types.RiderPlatform.Management.MultiModal.StageInfo
mkFRFSDataStatus mapping = return $ API.Types.RiderPlatform.Management.MultiModal.StageInfo mapping.stageName (mapStatus mapping.status)

mapStatus :: Status -> API.Types.RiderPlatform.Management.MultiModal.StageStatus
mapStatus Inprogress = API.Types.RiderPlatform.Management.MultiModal.INPROGRESS
mapStatus Completed = API.Types.RiderPlatform.Management.MultiModal.COMPLETED
mapStatus Failed = API.Types.RiderPlatform.Management.MultiModal.FAILED

postMultiModalMultimodalFrfsDataVersionIsReady :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.MultiModal.ReadyVersionReq -> Environment.Flow API.Types.RiderPlatform.Management.MultiModal.ReadyVersionsResp)
postMultiModalMultimodalFrfsDataVersionIsReady _merchantShortId _opCity req = do
  readyVersions <- QV.findAllReadyToApplyByMerchantOperatingCityAndVehicleTypeAndDataType True (Just $ Kernel.Types.Id.Id req.cityId) req.vehicleType (mapInputDataType req.inputDataType)
  versionLists <- traverse mkReadyVersionList readyVersions
  return $ API.Types.RiderPlatform.Management.MultiModal.ReadyVersionsResp versionLists

mapInputDataType :: API.Types.RiderPlatform.Management.MultiModal.RawDataType -> RawDataType
mapInputDataType API.Types.RiderPlatform.Management.MultiModal.GTFS_DATA = GTFS
mapInputDataType API.Types.RiderPlatform.Management.MultiModal.FARE_DATA = FARE

mkReadyVersionList :: (MonadFlow m) => Domain.Types.Version.Version -> m API.Types.RiderPlatform.Management.MultiModal.PreprocessFRFSDataResp
mkReadyVersionList version = return $ API.Types.RiderPlatform.Management.MultiModal.PreprocessFRFSDataResp version.id.getId version.versionTag

postMultiModalMultimodalFrfsDataVersionApply :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.MultiModal.ApplyVersionReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMultiModalMultimodalFrfsDataVersionApply _merchantShortId _opCity req = do
  unless (req.rolloutPercent >= 1 && req.rolloutPercent <= 100) $ throwError $ InternalError $ "Invalid rolloutPercent: " <> show req.rolloutPercent
  version <- QV.findByPrimaryKey (Kernel.Types.Id.Id req.versionId) >>= fromMaybeM (InternalError $ "Version with id: " <> show req.versionId <> " not found.")
  unless version.isReadyToApply $ throwError $ InternalError $ "Version with id: " <> show req.versionId <> " is not ready to apply."
  now <- getCurrentTime
  rolloutVersions <- QR.findAllByMerchantOperatingCityAndVehicleType (Just $ Kernel.Types.Id.Id req.cityId) req.vehicleType
  oldRolloutVersion <- maybe (throwError $ InternalError $ "No rollout version found for city: " <> show req.cityId <> " and vehicleType: " <> show req.vehicleType) return (listToMaybe <$> filter (\v -> v.versionTag /= req.versionTag) $ rolloutVersions)
  let updatedOldVersion = Domain.Types.Rollout.Rollout {id = oldRolloutVersion.id, inputDataType = oldRolloutVersion.inputDataType, vehicleType = oldRolloutVersion.vehicleType, versionTag = oldRolloutVersion.versionTag, merchantId = oldRolloutVersion.merchantId, merchantOperatingCityId = oldRolloutVersion.merchantOperatingCityId, createdAt = oldRolloutVersion.createdAt, percentage = 100 - req.rolloutPercent, updatedAt = now}
  id <- generateGUID
  let newVersion = Domain.Types.Rollout.Rollout {id, inputDataType = version.inputDataType, vehicleType = req.vehicleType, versionTag = req.versionTag, merchantId = version.merchantId, merchantOperatingCityId = version.merchantOperatingCityId, createdAt = now, percentage = req.rolloutPercent, updatedAt = now}
  void $ if updatedOldVersion.percentage == 0 then QR.deleteByVersionId updatedOldVersion.id else QR.updateByPrimaryKey updatedOldVersion
  void $ QR.create newVersion
  return Kernel.Types.APISuccess.Success
