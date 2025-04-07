{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.OperationHub where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.OperationHub
import qualified Domain.Types.OperationHubRequests
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data DriverOperationHubRequest = DriverOperationHubRequest
  { operationHubId :: Kernel.Types.Id.Id Domain.Types.OperationHub.OperationHub,
    registrationNo :: Kernel.Prelude.Text,
    requestType :: Domain.Types.OperationHubRequests.RequestType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
