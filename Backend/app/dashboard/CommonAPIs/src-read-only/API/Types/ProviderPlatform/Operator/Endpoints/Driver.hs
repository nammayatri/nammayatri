{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Operator.Endpoints.Driver where

import qualified Dashboard.Common
import qualified Dashboard.ProviderPlatform.Fleet.Driver
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import Data.Aeson
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Notification.FCM.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import Servant
import Servant.Client

newtype CreateDriversReq = CreateDriversReq {file :: Kernel.Prelude.FilePath}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateDriversReq where
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
  deriving stock (Generic, Eq)
  deriving anyclass (Kernel.Prelude.ToParamSchema, ToJSON, FromJSON, ToSchema)

data RequestType
  = ONBOARDING_INSPECTION
  | REGULAR_INSPECTION
  deriving stock (Generic, Eq)
  deriving anyclass (Kernel.Prelude.ToParamSchema, ToJSON, FromJSON, ToSchema)

data RespondHubRequest = RespondHubRequest {operationHubRequestId :: Kernel.Prelude.Text, operatorId :: Kernel.Prelude.Text, registrationNo :: Kernel.Prelude.Text, remarks :: Kernel.Prelude.Text, status :: RequestStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RespondHubRequest where
  hideSecrets = Kernel.Prelude.identity

data VerifyOperatorJoiningOtpReq = VerifyOperatorJoiningOtpReq
  { mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    otp :: Kernel.Prelude.Text,
    deviceToken :: Kernel.Prelude.Maybe Kernel.External.Notification.FCM.Types.FCMRecipientToken
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets VerifyOperatorJoiningOtpReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("driver" :> (GetDriverOperatorFetchHubRequests :<|> PostDriverOperatorRespondHubRequest :<|> PostDriverOperatorSendJoiningOtpHelper :<|> PostDriverOperatorVerifyJoiningOtpHelper :<|> PostDriverOperatorAddDriversHelper))

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

type PostDriverOperatorSendJoiningOtp =
  ( "operator" :> "sendJoiningOtp" :> ReqBody '[JSON] Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq
      :> Post
           '[JSON]
           Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes
  )

type PostDriverOperatorSendJoiningOtpHelper =
  ( "operator" :> "sendJoiningOtp" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq
      :> Post '[JSON] Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes
  )

type PostDriverOperatorVerifyJoiningOtp =
  ( "operator" :> "verifyJoiningOtp" :> QueryParam "authId" Kernel.Prelude.Text :> ReqBody '[JSON] VerifyOperatorJoiningOtpReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverOperatorVerifyJoiningOtpHelper =
  ( "operator" :> "verifyJoiningOtp" :> QueryParam "authId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> ReqBody '[JSON] VerifyOperatorJoiningOtpReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverOperatorAddDrivers =
  ( "operator" :> "addDrivers" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp CreateDriversReq
      :> Post
           '[JSON]
           Dashboard.ProviderPlatform.Fleet.Driver.APISuccessWithUnprocessedEntities
  )

type PostDriverOperatorAddDriversHelper =
  ( "operator" :> "addDrivers" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           CreateDriversReq
      :> Post '[JSON] Dashboard.ProviderPlatform.Fleet.Driver.APISuccessWithUnprocessedEntities
  )

data DriverAPIs = DriverAPIs
  { getDriverOperatorFetchHubRequests :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe RequestStatus -> Kernel.Prelude.Maybe RequestType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.OperationHub) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient OperationHubReqResp,
    postDriverOperatorRespondHubRequest :: RespondHubRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverOperatorSendJoiningOtp :: Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> EulerHS.Types.EulerClient Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes,
    postDriverOperatorVerifyJoiningOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> VerifyOperatorJoiningOtpReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverOperatorAddDrivers ::
      Kernel.Prelude.Text ->
      ( Data.ByteString.Lazy.ByteString,
        CreateDriversReq
      ) ->
      EulerHS.Types.EulerClient Dashboard.ProviderPlatform.Fleet.Driver.APISuccessWithUnprocessedEntities
  }

mkDriverAPIs :: (Client EulerHS.Types.EulerClient API -> DriverAPIs)
mkDriverAPIs driverClient = (DriverAPIs {..})
  where
    getDriverOperatorFetchHubRequests :<|> postDriverOperatorRespondHubRequest :<|> postDriverOperatorSendJoiningOtp :<|> postDriverOperatorVerifyJoiningOtp :<|> postDriverOperatorAddDrivers = driverClient

data DriverUserActionType
  = GET_DRIVER_OPERATOR_FETCH_HUB_REQUESTS
  | POST_DRIVER_OPERATOR_RESPOND_HUB_REQUEST
  | POST_DRIVER_OPERATOR_SEND_JOINING_OTP
  | POST_DRIVER_OPERATOR_VERIFY_JOINING_OTP
  | POST_DRIVER_OPERATOR_ADD_DRIVERS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''RequestStatus)

$(mkHttpInstancesForEnum ''RequestType)

$(Data.Singletons.TH.genSingletons [''DriverUserActionType])
