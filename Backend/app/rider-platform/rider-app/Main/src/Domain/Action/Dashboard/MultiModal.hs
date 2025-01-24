{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.MultiModal (postMultiModalMultimodalFrfsDataPreprocess, postMultiModalMultimodalFrfsDataStatus, postMultiModalMultimodalFrfsDataVersionIsReady, postMultiModalMultimodalFrfsDataVersionApply) where

import qualified API.Types.RiderPlatform.Management.MultiModal
import Data.OpenApi (ToSchema)
import Domain.Types.Extra.Rollout
import qualified Domain.Types.Merchant
import qualified Domain.Types.Stage as DTS
import qualified Domain.Types.Version
import Domain.Types.VersionStageMapping
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Storage.Queries.Stage as QS
import qualified Storage.Queries.Version as QV
import qualified Storage.Queries.VersionStageMapping as QVSM
import Tools.Auth
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
  versionLists <- traverse mkReadyVersion readyVersions
  return $ API.Types.RiderPlatform.Management.MultiModal.ReadyVersionsResp versionLists

mapInputDataType :: API.Types.RiderPlatform.Management.MultiModal.RawDataType -> RawDataType
mapInputDataType API.Types.RiderPlatform.Management.MultiModal.GTFS_DATA = GTFS
mapInputDataType API.Types.RiderPlatform.Management.MultiModal.FARE_DATA = FARE

mkReadyVersion :: (MonadFlow m) => Domain.Types.Version.Version -> m API.Types.RiderPlatform.Management.MultiModal.PreprocessFRFSDataResp
mkReadyVersion version = return $ API.Types.RiderPlatform.Management.MultiModal.PreprocessFRFSDataResp version.id.getId version.versionTag

postMultiModalMultimodalFrfsDataVersionApply :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.MultiModal.ApplyVersionReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMultiModalMultimodalFrfsDataVersionApply _merchantShortId _opCity _req = do error "Logic yet to be decided"
