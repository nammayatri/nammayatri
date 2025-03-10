{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.OperationHub where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.OperationHubRequests
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data DriverOperationHubRequest = DriverOperationHubRequest {operationHubId :: Kernel.Prelude.Text, requestType :: Domain.Types.OperationHubRequests.RequestType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
