{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Operator.Endpoints.Driver where

import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Kernel.Utils.TH
import Servant
import Servant.Client

data OperationHubDriverRequest = OperationHubDriverRequest
  { driverId :: Kernel.Prelude.Text,
    id :: Kernel.Prelude.Text,
    operationHubId :: Kernel.Prelude.Text,
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
  deriving stock (Generic, Eq)
  deriving anyclass (Kernel.Prelude.ToParamSchema, ToJSON, FromJSON, ToSchema)

data RequestType
  = ONBOARDING_INSPECTION
  | REGULAR_INSPECTION
  deriving stock (Generic, Eq)
  deriving anyclass (Kernel.Prelude.ToParamSchema, ToJSON, FromJSON, ToSchema)

data RespondHubRequest = RespondHubRequest {driverId :: Kernel.Prelude.Text, operationHubRequestId :: Kernel.Prelude.Text, operatorId :: Kernel.Prelude.Text, remarks :: Kernel.Prelude.Text, status :: RequestStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RespondHubRequest where
  hideSecrets = Kernel.Prelude.identity

type API = ("driver" :> (GetDriverOperatorFetchHubRequestsHelper :<|> PostDriverOperatorRespondHubRequestHelper))

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
      :> Get
           '[JSON]
           OperationHubReqResp
  )

type GetDriverOperatorFetchHubRequestsHelper =
  ( "operator" :> "fetch" :> "hubRequests" :> QueryParam "mbFrom" Kernel.Prelude.UTCTime
      :> QueryParam
           "mbTo"
           Kernel.Prelude.UTCTime
      :> QueryParam "mbStatus" RequestStatus
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
      :> Get
           '[JSON]
           OperationHubReqResp
  )

type PostDriverOperatorRespondHubRequest = ("operator" :> "respond" :> "hubRequest" :> ReqBody '[JSON] RespondHubRequest :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverOperatorRespondHubRequestHelper = ("operator" :> "respond" :> "hubRequest" :> ReqBody '[JSON] RespondHubRequest :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data DriverAPIs = DriverAPIs
  { getDriverOperatorFetchHubRequests :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe RequestStatus -> Kernel.Prelude.Maybe RequestType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient OperationHubReqResp,
    postDriverOperatorRespondHubRequest :: RespondHubRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkDriverAPIs :: (Client EulerHS.Types.EulerClient API -> DriverAPIs)
mkDriverAPIs driverClient = (DriverAPIs {..})
  where
    getDriverOperatorFetchHubRequests :<|> postDriverOperatorRespondHubRequest = driverClient

data DriverUserActionType
  = GET_DRIVER_OPERATOR_FETCH_HUB_REQUESTS
  | POST_DRIVER_OPERATOR_RESPOND_HUB_REQUEST
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''RequestStatus)

$(mkHttpInstancesForEnum ''RequestType)

$(Data.Singletons.TH.genSingletons [''DriverUserActionType])
