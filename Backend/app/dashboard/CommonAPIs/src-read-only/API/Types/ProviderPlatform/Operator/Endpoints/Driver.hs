{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Operator.Endpoints.Driver where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.Driver
import qualified API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding
import qualified Dashboard.Common
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time.Calendar
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Notification.FCM.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import Servant
import Servant.Client

data AllTimeOperatorAnalyticsRes = AllTimeOperatorAnalyticsRes {rating :: Kernel.Prelude.Maybe Kernel.Prelude.Double, cancellationRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double, acceptanceRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverInfo = DriverInfo
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    firstName :: Kernel.Prelude.Text,
    middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Endpoints.Driver.DriverMode,
    isActive :: Kernel.Prelude.Bool,
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    vehicle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isRcActive :: Kernel.Prelude.Bool,
    documents :: API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding.StatusRes
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverInfoResp = DriverInfoResp {listItem :: [DriverInfo], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverOperationHubRequest = DriverOperationHubRequest {creatorId :: Kernel.Prelude.Text, operationHubId :: Kernel.Types.Id.Id OperationHub, registrationNo :: Kernel.Prelude.Text, requestType :: RequestType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DriverOperationHubRequest where
  hideSecrets = Kernel.Prelude.identity

data FilteredOperatorAnalyticsRes = FilteredOperatorAnalyticsRes
  { activeDriver :: Kernel.Prelude.Int,
    driverEnabled :: Kernel.Prelude.Int,
    greaterThanOneRide :: Kernel.Prelude.Int,
    greaterThanTenRide :: Kernel.Prelude.Int,
    greaterThanFiftyRide :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OperationHub = OperationHub
  { address :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id OperationHub,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OperationHubDriverRequest = OperationHubDriverRequest
  { creatorPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Prelude.Text,
    operationHubId :: Kernel.Types.Id.Id OperationHub,
    operationHubName :: Kernel.Prelude.Text,
    rcId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    registrationNo :: Kernel.Prelude.Text,
    requestStatus :: RequestStatus,
    requestTime :: Kernel.Prelude.UTCTime,
    requestType :: RequestType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OperationHubReqResp = OperationHubReqResp {requests :: [OperationHubDriverRequest], summary :: Dashboard.Common.Summary}
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

data RespondHubRequest = RespondHubRequest {operationHubRequestId :: Kernel.Prelude.Text, operatorId :: Kernel.Prelude.Text, remarks :: Kernel.Prelude.Text, status :: RequestStatus}
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

type API = ("driver" :> (GetDriverOperatorFetchHubRequests :<|> GetDriverOperationGetAllHubs :<|> PostDriverOperatorRespondHubRequest :<|> PostDriverOperatorCreateRequest :<|> GetDriverOperatorListHelper :<|> PostDriverOperatorSendJoiningOtpHelper :<|> PostDriverOperatorVerifyJoiningOtpHelper :<|> GetDriverOperatorDashboardAnalyticsAllTimeHelper :<|> GetDriverOperatorDashboardAnalyticsHelper))

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
           (Kernel.Types.Id.Id OperationHub)
      :> QueryParam
           "mbOperationHubName"
           Kernel.Prelude.Text
      :> QueryParam
           "mbRegistrationNo"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           OperationHubReqResp
  )

type GetDriverOperationGetAllHubs = ("operation" :> "getAllHubs" :> Get '[JSON] [OperationHub])

type PostDriverOperatorRespondHubRequest = ("operator" :> "respond" :> "hubRequest" :> ReqBody '[JSON] RespondHubRequest :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverOperatorCreateRequest = ("operator" :> "createRequest" :> ReqBody '[JSON] DriverOperationHubRequest :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetDriverOperatorList =
  ( "operator" :> "list" :> QueryParam "isActive" Kernel.Prelude.Bool :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "vehicleNo" Kernel.Prelude.Text
      :> QueryParam "mbSearchString" Kernel.Prelude.Text
      :> QueryParam
           "onlyMandatoryDocs"
           Kernel.Prelude.Bool
      :> Get
           '[JSON]
           DriverInfoResp
  )

type GetDriverOperatorListHelper =
  ( "operator" :> "list" :> QueryParam "isActive" Kernel.Prelude.Bool :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "vehicleNo" Kernel.Prelude.Text
      :> QueryParam
           "mbSearchString"
           Kernel.Prelude.Text
      :> QueryParam
           "onlyMandatoryDocs"
           Kernel.Prelude.Bool
      :> MandatoryQueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           DriverInfoResp
  )

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

type GetDriverOperatorDashboardAnalyticsAllTime = ("operator" :> "dashboard" :> "analytics" :> "allTime" :> Get '[JSON] AllTimeOperatorAnalyticsRes)

type GetDriverOperatorDashboardAnalyticsAllTimeHelper =
  ( Capture "requestorId" Kernel.Prelude.Text :> "operator" :> "dashboard" :> "analytics" :> "allTime"
      :> Get
           '[JSON]
           AllTimeOperatorAnalyticsRes
  )

type GetDriverOperatorDashboardAnalytics =
  ( "operator" :> "dashboard" :> "analytics" :> MandatoryQueryParam "from" Data.Time.Calendar.Day
      :> MandatoryQueryParam
           "to"
           Data.Time.Calendar.Day
      :> Get '[JSON] FilteredOperatorAnalyticsRes
  )

type GetDriverOperatorDashboardAnalyticsHelper =
  ( Capture "requestorId" Kernel.Prelude.Text :> "operator" :> "dashboard" :> "analytics"
      :> MandatoryQueryParam
           "from"
           Data.Time.Calendar.Day
      :> MandatoryQueryParam "to" Data.Time.Calendar.Day
      :> Get '[JSON] FilteredOperatorAnalyticsRes
  )

data DriverAPIs = DriverAPIs
  { getDriverOperatorFetchHubRequests :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe RequestStatus -> Kernel.Prelude.Maybe RequestType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id OperationHub) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient OperationHubReqResp,
    getDriverOperationGetAllHubs :: EulerHS.Types.EulerClient [OperationHub],
    postDriverOperatorRespondHubRequest :: RespondHubRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverOperatorCreateRequest :: DriverOperationHubRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverOperatorList :: Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient DriverInfoResp,
    postDriverOperatorSendJoiningOtp :: Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> EulerHS.Types.EulerClient Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes,
    postDriverOperatorVerifyJoiningOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> VerifyOperatorJoiningOtpReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverOperatorDashboardAnalyticsAllTime :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient AllTimeOperatorAnalyticsRes,
    getDriverOperatorDashboardAnalytics :: Kernel.Prelude.Text -> Data.Time.Calendar.Day -> Data.Time.Calendar.Day -> EulerHS.Types.EulerClient FilteredOperatorAnalyticsRes
  }

mkDriverAPIs :: (Client EulerHS.Types.EulerClient API -> DriverAPIs)
mkDriverAPIs driverClient = (DriverAPIs {..})
  where
    getDriverOperatorFetchHubRequests :<|> getDriverOperationGetAllHubs :<|> postDriverOperatorRespondHubRequest :<|> postDriverOperatorCreateRequest :<|> getDriverOperatorList :<|> postDriverOperatorSendJoiningOtp :<|> postDriverOperatorVerifyJoiningOtp :<|> getDriverOperatorDashboardAnalyticsAllTime :<|> getDriverOperatorDashboardAnalytics = driverClient

data DriverUserActionType
  = GET_DRIVER_OPERATOR_FETCH_HUB_REQUESTS
  | GET_DRIVER_OPERATION_GET_ALL_HUBS
  | POST_DRIVER_OPERATOR_RESPOND_HUB_REQUEST
  | POST_DRIVER_OPERATOR_CREATE_REQUEST
  | GET_DRIVER_OPERATOR_LIST
  | POST_DRIVER_OPERATOR_SEND_JOINING_OTP
  | POST_DRIVER_OPERATOR_VERIFY_JOINING_OTP
  | GET_DRIVER_OPERATOR_DASHBOARD_ANALYTICS_ALL_TIME
  | GET_DRIVER_OPERATOR_DASHBOARD_ANALYTICS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''RequestStatus)

$(mkHttpInstancesForEnum ''RequestType)

$(Data.Singletons.TH.genSingletons [''DriverUserActionType])
