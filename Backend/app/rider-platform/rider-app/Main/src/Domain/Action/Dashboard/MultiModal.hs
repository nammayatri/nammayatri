{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.MultiModal (postMultiModalMultimodalFrfsDataPreprocess, postMultiModalMultimodalFrfsDataStatus, postMultiModalMultimodalFrfsDataVersionIsReady, postMultiModalMultimodalFrfsDataVersionApply) where

import qualified API.Types.RiderPlatform.Management.MultiModal
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Stage as DTS
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Storage.Queries.Stage as QS
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
postMultiModalMultimodalFrfsDataStatus _merchantShortId _opCity _req = do
  -- stage <- QS.findByMerchantOperatingCityAndVehicleTypeAndStageName req.operatingCityId req.vehicleType DTS.UPLOAD >>= fromMaybeM (InternalError $ "Upload Stage for " <> show req.operatingCityId <> " and " <> show req.vehicleType <> " not found")
  error "Logic yet to be decided"

postMultiModalMultimodalFrfsDataVersionIsReady :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.MultiModal.ReadyVersionReq -> Environment.Flow API.Types.RiderPlatform.Management.MultiModal.ReadyVersionsResp)
postMultiModalMultimodalFrfsDataVersionIsReady _merchantShortId _opCity _req = do error "Logic yet to be decided"

postMultiModalMultimodalFrfsDataVersionApply :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.MultiModal.ApplyVersionReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMultiModalMultimodalFrfsDataVersionApply _merchantShortId _opCity _req = do error "Logic yet to be decided"
