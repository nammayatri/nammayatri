module ConfigPilotFrontend.Types where

import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)

newtype TSServiceConfig = TSServiceConfig
  { url :: BaseUrl
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

data TSServiceValidateStatus = VALID_CONFIG | INVALID_CONFIG | INVALID_REQUEST deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

newtype TSServiceValidateResp = TSServiceValidateResp
  { status :: TSServiceValidateStatus
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

type HasTSServiceConfig m r = (HasFlowEnv m r '["tsServiceConfig" ::: TSServiceConfig])
