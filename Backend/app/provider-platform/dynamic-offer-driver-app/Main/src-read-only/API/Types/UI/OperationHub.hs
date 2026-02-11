{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.OperationHub where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.OperationHub
import qualified Domain.Types.OperationHubRequests
import qualified Domain.Types.Person
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data DriverOperationHubRequest = DriverOperationHubRequest
  { creatorId :: Kernel.Prelude.Text,
    driverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    operationHubId :: Kernel.Types.Id.Id Domain.Types.OperationHub.OperationHub,
    registrationNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    requestType :: Domain.Types.OperationHubRequests.RequestType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OperationHubDriverRequest = OperationHubDriverRequest
  { driverPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Prelude.Text,
    operationHubId :: Kernel.Types.Id.Id Domain.Types.OperationHub.OperationHub,
    operationHubName :: Kernel.Prelude.Text,
    registrationNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    requestStatus :: Domain.Types.OperationHubRequests.RequestStatus,
    requestTime :: Kernel.Prelude.UTCTime,
    requestType :: Domain.Types.OperationHubRequests.RequestType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype OperationHubRequestsResp = OperationHubRequestsResp {requests :: [OperationHubDriverRequest]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
