{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.MultiModal where

import qualified BecknV2.FRFS.Enums
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Prelude
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data ApplyVersionReq = ApplyVersionReq
  { cityId :: Kernel.Prelude.Text,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    versionId :: Kernel.Prelude.Text,
    versionTag :: Kernel.Prelude.Text,
    rolloutPercent :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ApplyVersionReq where
  hideSecrets = Kernel.Prelude.identity

data FRFSDataStatusReq = FRFSDataStatusReq {versionId :: Kernel.Prelude.Text, operatingCityId :: Kernel.Prelude.Text, vehicleType :: BecknV2.FRFS.Enums.VehicleCategory}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FRFSDataStatusReq where
  hideSecrets = Kernel.Prelude.identity

data FRFSDataStatusResp = FRFSDataStatusResp {versionId :: Kernel.Prelude.Text, status :: [StageInfo]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FileFormat
  = GTFS
  | JSON
  | CSV
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PreprocessFRFSDataReq = PreprocessFRFSDataReq {vehicleType :: BecknV2.FRFS.Enums.VehicleCategory, fileFormat :: FileFormat, file :: EulerHS.Prelude.FilePath, inputDataType :: RawDataType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PreprocessFRFSDataReq where
  hideSecrets = Kernel.Prelude.identity

data PreprocessFRFSDataResp = PreprocessFRFSDataResp {versionId :: Kernel.Prelude.Text, versionTag :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RawDataType
  = GTFS_DATA
  | FARE_DATA
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReadyVersionReq = ReadyVersionReq {cityId :: Kernel.Prelude.Text, vehicleType :: BecknV2.FRFS.Enums.VehicleCategory, inputDataType :: RawDataType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ReadyVersionReq where
  hideSecrets = Kernel.Prelude.identity

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

type PostMultiModalMultimodalFrfsDataPreprocess = ("multimodal" :> "frfs" :> "data" :> "preprocess" :> ReqBody '[JSON] PreprocessFRFSDataReq :> Post '[JSON] PreprocessFRFSDataResp)

type PostMultiModalMultimodalFrfsDataStatus = ("multimodal" :> "frfs" :> "data" :> "status" :> ReqBody '[JSON] FRFSDataStatusReq :> Post '[JSON] FRFSDataStatusResp)

type PostMultiModalMultimodalFrfsDataVersionIsReady = ("multimodal" :> "frfs" :> "data" :> "version" :> "isReady" :> ReqBody '[JSON] ReadyVersionReq :> Post '[JSON] ReadyVersionsResp)

type PostMultiModalMultimodalFrfsDataVersionApply = ("multimodal" :> "frfs" :> "data" :> "version" :> "apply" :> ReqBody '[JSON] ApplyVersionReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data MultiModalAPIs = MultiModalAPIs
  { postMultiModalMultimodalFrfsDataPreprocess :: PreprocessFRFSDataReq -> EulerHS.Types.EulerClient PreprocessFRFSDataResp,
    postMultiModalMultimodalFrfsDataStatus :: FRFSDataStatusReq -> EulerHS.Types.EulerClient FRFSDataStatusResp,
    postMultiModalMultimodalFrfsDataVersionIsReady :: ReadyVersionReq -> EulerHS.Types.EulerClient ReadyVersionsResp,
    postMultiModalMultimodalFrfsDataVersionApply :: ApplyVersionReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
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
