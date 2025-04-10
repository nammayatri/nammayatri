{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Operator.Endpoints.Driver where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding
import qualified Dashboard.Common
import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import Servant
import Servant.Client

data DriverInfo = DriverInfo
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    firstName :: Kernel.Prelude.Text,
    middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isActive :: Kernel.Prelude.Bool,
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    vehicle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documents :: API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding.StatusRes
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverOperationHubRequest = DriverOperationHubRequest
  { creatorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverId :: Kernel.Prelude.Text,
    operationHubId :: Kernel.Types.Id.Id Dashboard.Common.OperationHub,
    registrationNo :: Kernel.Prelude.Text,
    requestType :: RequestType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DriverOperationHubRequest where
  hideSecrets = Kernel.Prelude.identity

data OperationHubDriverRequest = OperationHubDriverRequest
  { driverId :: Kernel.Prelude.Text,
    id :: Kernel.Prelude.Text,
    operationHubId :: Kernel.Types.Id.Id Dashboard.Common.OperationHub,
    registrationNo :: Kernel.Prelude.Text,
    requestStatus :: RequestStatus,
    requestTime :: Kernel.Prelude.UTCTime,
    requestType :: RequestType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype OperationHubReqResp = OperationHubReqResp {requests :: [OperationHubDriverRequest]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RequestStatus
  = PENDING
  | APPROVED
  | REJECTED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data RequestType
  = ONBOARDING_INSPECTION
  | REGULAR_INSPECTION
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data RespondHubRequest = RespondHubRequest {operationHubRequestId :: Kernel.Prelude.Text, operatorId :: Kernel.Prelude.Text, registrationNo :: Kernel.Prelude.Text, remarks :: Kernel.Prelude.Text, status :: RequestStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RespondHubRequest where
  hideSecrets = Kernel.Prelude.identity

type API = ("driver" :> (GetDriverOperatorFetchHubRequests :<|> PostDriverOperatorRespondHubRequest :<|> PostDriverOperatorCreateRequest :<|> GetDriverOperatorListHelper))

type GetDriverOperatorFetchHubRequests =
  ( "operator" :> "fetch" :> "hubRequests" :> QueryParam "mbFrom" Kernel.Prelude.UTCTime :> QueryParam "mbTo" Kernel.Prelude.UTCTime
      :> QueryParam
           "mbStatus"
           RequestStatus
      :> QueryParam "mbReqType" RequestType
      :> QueryParam
           "mbLimit"
           Kernel.Prelude.Int
      :> QueryParam
           "mbOffset"
           Kernel.Prelude.Int
      :> QueryParam
           "mbDriverId"
           Kernel.Prelude.Text
      :> QueryParam
           "mbMobileNumber"
           Kernel.Prelude.Text
      :> QueryParam
           "mbOperationHubId"
           (Kernel.Types.Id.Id Dashboard.Common.OperationHub)
      :> QueryParam
           "mbRegistrationNo"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           OperationHubReqResp
  )

type PostDriverOperatorRespondHubRequest = ("operator" :> "respond" :> "hubRequest" :> ReqBody '[JSON] RespondHubRequest :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverOperatorCreateRequest = ("operator" :> "createRequest" :> ReqBody '[JSON] DriverOperationHubRequest :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetDriverOperatorList =
  ( "operator" :> "list" :> QueryParam "isActive" Kernel.Prelude.Bool :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> Get
           '[JSON]
           [DriverInfo]
  )

type GetDriverOperatorListHelper =
  ( "operator" :> "list" :> QueryParam "isActive" Kernel.Prelude.Bool :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> Get '[JSON] [DriverInfo]
  )

data DriverAPIs = DriverAPIs
  { getDriverOperatorFetchHubRequests :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe RequestStatus -> Kernel.Prelude.Maybe RequestType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.OperationHub) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient OperationHubReqResp,
    postDriverOperatorRespondHubRequest :: RespondHubRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverOperatorCreateRequest :: DriverOperationHubRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverOperatorList :: Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient [DriverInfo]
  }

mkDriverAPIs :: (Client EulerHS.Types.EulerClient API -> DriverAPIs)
mkDriverAPIs driverClient = (DriverAPIs {..})
  where
    getDriverOperatorFetchHubRequests :<|> postDriverOperatorRespondHubRequest :<|> postDriverOperatorCreateRequest :<|> getDriverOperatorList = driverClient

data DriverUserActionType
  = GET_DRIVER_OPERATOR_FETCH_HUB_REQUESTS
  | POST_DRIVER_OPERATOR_RESPOND_HUB_REQUEST
  | POST_DRIVER_OPERATOR_CREATE_REQUEST
  | GET_DRIVER_OPERATOR_LIST
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''RequestStatus)

$(mkHttpInstancesForEnum ''RequestType)

$(Data.Singletons.TH.genSingletons [''DriverUserActionType])
