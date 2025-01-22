{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.MultiModal where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import Servant
import Servant.Client

data FRFSDataStatusResp = FRFSDataStatusResp {versionId :: Kernel.Prelude.Text, status :: StageInfo}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FileFormat
  = GTFS
  | JSON
  | CSV
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PreprocessFRFSDataResp = PreprocessFRFSDataResp {versionId :: Kernel.Prelude.Text, versionTag :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReadyVersionsResp = ReadyVersionsResp {readyVersions :: [PreprocessFRFSDataResp]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StageInfo = StageInfo {stageName :: Kernel.Prelude.Text, stageStatus :: StageStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StageStatus
  = INPROGRESS
  | COMPLETED
  | FAILED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("multiModal" :> (PostMultiModalMultimodalFrfsDataPreprocess :<|> PostMultiModalMultimodalFrfsDataStatus :<|> PostMultiModalMultimodalFrfsDataVersionIsReady :<|> PostMultiModalMultimodalFrfsDataVersionApply))

type PostMultiModalMultimodalFrfsDataPreprocess = ("multimodal" :> "frfs" :> "data" :> "preprocess" :> Post '[JSON] PreprocessFRFSDataResp)

type PostMultiModalMultimodalFrfsDataStatus = ("multimodal" :> "frfs" :> "data" :> "status" :> Post '[JSON] FRFSDataStatusResp)

type PostMultiModalMultimodalFrfsDataVersionIsReady = ("multimodal" :> "frfs" :> "data" :> "version" :> "isReady" :> Post '[JSON] ReadyVersionsResp)

type PostMultiModalMultimodalFrfsDataVersionApply = ("multimodal" :> "frfs" :> "data" :> "version" :> "apply" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data MultiModalAPIs = MultiModalAPIs
  { postMultiModalMultimodalFrfsDataPreprocess :: EulerHS.Types.EulerClient PreprocessFRFSDataResp,
    postMultiModalMultimodalFrfsDataStatus :: EulerHS.Types.EulerClient FRFSDataStatusResp,
    postMultiModalMultimodalFrfsDataVersionIsReady :: EulerHS.Types.EulerClient ReadyVersionsResp,
    postMultiModalMultimodalFrfsDataVersionApply :: EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkMultiModalAPIs :: (Client EulerHS.Types.EulerClient API -> MultiModalAPIs)
mkMultiModalAPIs multiModalClient = (MultiModalAPIs {..})
  where
    postMultiModalMultimodalFrfsDataPreprocess :<|> postMultiModalMultimodalFrfsDataStatus :<|> postMultiModalMultimodalFrfsDataVersionIsReady :<|> postMultiModalMultimodalFrfsDataVersionApply = multiModalClient

data MultiModalUserActionType
  = POST_MULTI_MODAL_MULTIMODAL_FRFS_DATA_PREPROCESS
  | POST_MULTI_MODAL_MULTIMODAL_FRFS_DATA_STATUS
  | POST_MULTI_MODAL_MULTIMODAL_FRFS_DATA_VERSION_IS_READY
  | POST_MULTI_MODAL_MULTIMODAL_FRFS_DATA_VERSION_APPLY
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''MultiModalUserActionType])
