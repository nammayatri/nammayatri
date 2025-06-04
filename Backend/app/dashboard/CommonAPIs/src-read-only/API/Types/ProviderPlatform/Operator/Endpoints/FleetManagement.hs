{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Operator.Endpoints.FleetManagement where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding
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
    isActive :: Kernel.Prelude.Bool,
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    vehicleCount :: Kernel.Prelude.Int,
    verified :: Kernel.Prelude.Bool,
    documents :: API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding.StatusRes
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetInfoRes = FleetInfoRes {listItem :: [FleetInfo], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOwnerCreateReq = FleetOwnerCreateReq {mobileNumber :: Kernel.Prelude.Text, mobileCountryCode :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype FleetOwnerCreateRes = FleetOwnerCreateRes {personId :: Kernel.Types.Id.Id Dashboard.Common.Person}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FleetOwnerCreateRes where
  hideSecrets = Kernel.Prelude.identity

data FleetOwnerRegisterReq = FleetOwnerRegisterReq
  { firstName :: Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Text,
    personId :: Kernel.Types.Id.Id Dashboard.Common.Person,
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetType :: Kernel.Prelude.Maybe FleetType,
    panNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gstNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    businessLicenseNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    panImageId1 :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    panImageId2 :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gstCertificateImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    businessLicenseImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOwnerRegisterTReq = FleetOwnerRegisterTReq {firstName :: Kernel.Prelude.Text, lastName :: Kernel.Prelude.Text, personId :: Kernel.Types.Id.Id Dashboard.Common.Person, fleetType :: Kernel.Prelude.Maybe FleetType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOwnerSendOtpReq = FleetOwnerSendOtpReq {mobileNumber :: Kernel.Prelude.Text, mobileCountryCode :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOwnerSendOtpRes = FleetOwnerSendOtpRes {fleetOwnerId :: Kernel.Types.Id.Id Dashboard.Common.Person, name :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FleetOwnerSendOtpRes where
  hideSecrets = Kernel.Prelude.identity

newtype FleetOwnerUpdateRes = FleetOwnerUpdateRes {enabled :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOwnerVerifyOtpReq = FleetOwnerVerifyOtpReq {fleetOwnerId :: Kernel.Types.Id.Id Dashboard.Common.Person, otp :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FleetOwnerVerifyOtpReq where
  hideSecrets = Kernel.Prelude.identity

data FleetType
  = RENTAL_FLEET
  | NORMAL_FLEET
  | BUSINESS_FLEET
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("operator" :> (GetFleetManagementFleetsHelper :<|> PostFleetManagementFleetCreateHelper :<|> PostFleetManagementFleetRegisterHelper :<|> PostFleetManagementFleetLinkSendOtpHelper :<|> PostFleetManagementFleetLinkVerifyOtpHelper :<|> PostFleetManagementFleetUnlinkHelper))

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

type PostFleetManagementFleetCreate = ("fleet" :> "create" :> ReqBody '[JSON] FleetOwnerCreateReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostFleetManagementFleetCreateHelper =
  ( "fleet" :> "create" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] FleetOwnerCreateReq
      :> Post
           '[JSON]
           FleetOwnerCreateRes
  )

type PostFleetManagementFleetRegister = ("fleet" :> "register" :> ReqBody '[JSON] FleetOwnerRegisterReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostFleetManagementFleetRegisterHelper =
  ( "fleet" :> "register" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] FleetOwnerRegisterReq
      :> Post
           '[JSON]
           FleetOwnerUpdateRes
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

data FleetManagementAPIs = FleetManagementAPIs
  { getFleetManagementFleets :: Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient FleetInfoRes,
    postFleetManagementFleetCreate :: Kernel.Prelude.Text -> FleetOwnerCreateReq -> EulerHS.Types.EulerClient FleetOwnerCreateRes,
    postFleetManagementFleetRegister :: Kernel.Prelude.Text -> FleetOwnerRegisterReq -> EulerHS.Types.EulerClient FleetOwnerUpdateRes,
    postFleetManagementFleetLinkSendOtp :: Kernel.Prelude.Text -> FleetOwnerSendOtpReq -> EulerHS.Types.EulerClient FleetOwnerSendOtpRes,
    postFleetManagementFleetLinkVerifyOtp :: Kernel.Prelude.Text -> FleetOwnerVerifyOtpReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postFleetManagementFleetUnlink :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkFleetManagementAPIs :: (Client EulerHS.Types.EulerClient API -> FleetManagementAPIs)
mkFleetManagementAPIs fleetManagementClient = (FleetManagementAPIs {..})
  where
    getFleetManagementFleets :<|> postFleetManagementFleetCreate :<|> postFleetManagementFleetRegister :<|> postFleetManagementFleetLinkSendOtp :<|> postFleetManagementFleetLinkVerifyOtp :<|> postFleetManagementFleetUnlink = fleetManagementClient

data FleetManagementUserActionType
  = GET_FLEET_MANAGEMENT_FLEETS
  | POST_FLEET_MANAGEMENT_FLEET_CREATE
  | POST_FLEET_MANAGEMENT_FLEET_REGISTER
  | POST_FLEET_MANAGEMENT_FLEET_LINK_SEND_OTP
  | POST_FLEET_MANAGEMENT_FLEET_LINK_VERIFY_OTP
  | POST_FLEET_MANAGEMENT_FLEET_UNLINK
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''FleetManagementUserActionType])
