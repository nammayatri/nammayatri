{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Operator.Endpoints.FleetManagement where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding
import qualified API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2
import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data FleetInfo = FleetInfo
  { id :: Kernel.Types.Id.Id Dashboard.Common.Person,
    name :: Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    isActive :: Kernel.Prelude.Bool,
    fleetType :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetType,
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    vehicleCount :: Kernel.Prelude.Int,
    verified :: Kernel.Prelude.Bool,
    documents :: API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding.StatusRes,
    registeredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetInfoRes = FleetInfoRes {listItem :: [FleetInfo], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetMemberAssociationCreateReq = FleetMemberAssociationCreateReq
  { fleetMemberId :: Kernel.Prelude.Text,
    fleetOwnerId :: Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    isFleetOwner :: Kernel.Prelude.Bool,
    level :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    parentGroupCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    groupCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FleetMemberAssociationCreateReq where
  hideSecrets = Kernel.Prelude.identity

data FleetOwnerSendOtpReq = FleetOwnerSendOtpReq {mobileNumber :: Kernel.Prelude.Text, mobileCountryCode :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOwnerSendOtpRes = FleetOwnerSendOtpRes {fleetOwnerId :: Kernel.Types.Id.Id Dashboard.Common.Person, name :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FleetOwnerSendOtpRes where
  hideSecrets = Kernel.Prelude.identity

data FleetOwnerVerifyOtpReq = FleetOwnerVerifyOtpReq {fleetOwnerId :: Kernel.Types.Id.Id Dashboard.Common.Person, otp :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FleetOwnerVerifyOtpReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("operator" :> (GetFleetManagementFleetsHelper :<|> PostFleetManagementFleetCreateHelper :<|> PostFleetManagementFleetRegisterHelper :<|> PostFleetManagementFleetLinkSendOtpHelper :<|> PostFleetManagementFleetLinkVerifyOtpHelper :<|> PostFleetManagementFleetUnlinkHelper :<|> PostFleetManagementFleetMemberAssociationCreateHelper))

type GetFleetManagementFleets =
  ( "fleets" :> QueryParam "isActive" Kernel.Prelude.Bool :> QueryParam "verified" Kernel.Prelude.Bool :> QueryParam "enabled" Kernel.Prelude.Bool
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "mbSearchString"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           FleetInfoRes
  )

type GetFleetManagementFleetsHelper =
  ( "fleets" :> QueryParam "isActive" Kernel.Prelude.Bool :> QueryParam "verified" Kernel.Prelude.Bool
      :> QueryParam
           "enabled"
           Kernel.Prelude.Bool
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "mbSearchString"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           FleetInfoRes
  )

type PostFleetManagementFleetCreate =
  ( "fleet" :> "create" :> ReqBody '[JSON] API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerLoginReqV2
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostFleetManagementFleetCreateHelper =
  ( "fleet" :> "create" :> QueryParam "enabled" Kernel.Prelude.Bool :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerLoginReqV2
      :> Post
           '[JSON]
           API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerLoginResV2
  )

type PostFleetManagementFleetRegister =
  ( "fleet" :> "register" :> ReqBody '[JSON] API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerRegisterReqV2
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostFleetManagementFleetRegisterHelper =
  ( "fleet" :> "register" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerRegisterReqV2
      :> Post
           '[JSON]
           API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerRegisterResV2
  )

type PostFleetManagementFleetLinkSendOtp = ("fleet" :> "link" :> "sendOtp" :> ReqBody '[JSON] FleetOwnerSendOtpReq :> Post '[JSON] FleetOwnerSendOtpRes)

type PostFleetManagementFleetLinkSendOtpHelper =
  ( "fleet" :> "link" :> "sendOtp" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] FleetOwnerSendOtpReq
      :> Post
           '[JSON]
           FleetOwnerSendOtpRes
  )

type PostFleetManagementFleetLinkVerifyOtp = ("fleet" :> "link" :> "verifyOtp" :> ReqBody '[JSON] FleetOwnerVerifyOtpReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostFleetManagementFleetLinkVerifyOtpHelper =
  ( "fleet" :> "link" :> "verifyOtp" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           FleetOwnerVerifyOtpReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostFleetManagementFleetUnlink = ("fleet" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> "unlink" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostFleetManagementFleetUnlinkHelper =
  ( "fleet" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> "unlink" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostFleetManagementFleetMemberAssociationCreate =
  ( "fleet" :> "member" :> "association" :> "create" :> ReqBody '[JSON] FleetMemberAssociationCreateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostFleetManagementFleetMemberAssociationCreateHelper =
  ( "fleet" :> "member" :> "association" :> "create" :> ReqBody '[JSON] FleetMemberAssociationCreateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data FleetManagementAPIs = FleetManagementAPIs
  { getFleetManagementFleets :: Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient FleetInfoRes,
    postFleetManagementFleetCreate :: Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerLoginReqV2 -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerLoginResV2,
    postFleetManagementFleetRegister :: Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerRegisterReqV2 -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerRegisterResV2,
    postFleetManagementFleetLinkSendOtp :: Kernel.Prelude.Text -> FleetOwnerSendOtpReq -> EulerHS.Types.EulerClient FleetOwnerSendOtpRes,
    postFleetManagementFleetLinkVerifyOtp :: Kernel.Prelude.Text -> FleetOwnerVerifyOtpReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postFleetManagementFleetUnlink :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postFleetManagementFleetMemberAssociationCreate :: FleetMemberAssociationCreateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkFleetManagementAPIs :: (Client EulerHS.Types.EulerClient API -> FleetManagementAPIs)
mkFleetManagementAPIs fleetManagementClient = (FleetManagementAPIs {..})
  where
    getFleetManagementFleets :<|> postFleetManagementFleetCreate :<|> postFleetManagementFleetRegister :<|> postFleetManagementFleetLinkSendOtp :<|> postFleetManagementFleetLinkVerifyOtp :<|> postFleetManagementFleetUnlink :<|> postFleetManagementFleetMemberAssociationCreate = fleetManagementClient

data FleetManagementUserActionType
  = GET_FLEET_MANAGEMENT_FLEETS
  | POST_FLEET_MANAGEMENT_FLEET_CREATE
  | POST_FLEET_MANAGEMENT_FLEET_REGISTER
  | POST_FLEET_MANAGEMENT_FLEET_LINK_SEND_OTP
  | POST_FLEET_MANAGEMENT_FLEET_LINK_VERIFY_OTP
  | POST_FLEET_MANAGEMENT_FLEET_UNLINK
  | POST_FLEET_MANAGEMENT_FLEET_MEMBER_ASSOCIATION_CREATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''FleetManagementUserActionType])
