{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2 where

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

data FleetMemberAssociation = FleetMemberAssociation
  { fleetMemberId :: Kernel.Prelude.Text,
    fleetOwnerId :: Kernel.Prelude.Text,
    level :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    parentGroupCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    groupCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    isFleetOwner :: Kernel.Prelude.Bool,
    enabled :: Kernel.Prelude.Bool,
    associatedTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetMemberAssociationRes = FleetMemberAssociationRes {fleetOwnerId :: Kernel.Prelude.Text, listItem :: [FleetMemberAssociation], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOwnerLoginReqV2 = FleetOwnerLoginReqV2 {mobileNumber :: Kernel.Prelude.Text, mobileCountryCode :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype FleetOwnerLoginResV2 = FleetOwnerLoginResV2 {personId :: Kernel.Types.Id.Id Dashboard.Common.Person}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FleetOwnerLoginResV2 where
  hideSecrets = Kernel.Prelude.identity

data FleetOwnerRegisterReqV2 = FleetOwnerRegisterReqV2
  { firstName :: Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Text,
    personId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Person),
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetType :: Kernel.Prelude.Maybe FleetType,
    businessLicenseNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    businessLicenseImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    operatorReferralCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    adminApprovalRequired :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    setIsEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype FleetOwnerRegisterResV2 = FleetOwnerRegisterResV2 {enabled :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOwnerRegisterTReqV2 = FleetOwnerRegisterTReqV2
  { firstName :: Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Text,
    personId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Person),
    fleetType :: Kernel.Prelude.Maybe FleetType,
    operatorReferralCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    adminApprovalRequired :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOwnerVerifyReqV2 = FleetOwnerVerifyReqV2 {mobileNumber :: Kernel.Prelude.Text, mobileCountryCode :: Kernel.Prelude.Text, otp :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype FleetOwnerVerifyResV2 = FleetOwnerVerifyResV2 {authToken :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetType
  = RENTAL_FLEET
  | NORMAL_FLEET
  | BUSINESS_FLEET
  | FLEET_MANAGER
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("fleet" :> (PostRegistrationV2V2LoginOtpHelper :<|> PostRegistrationV2V2VerifyOtpHelper :<|> PostRegistrationV2V2RegisterHelper :<|> GetRegistrationV2FleetMembersHelper :<|> PostRegistrationV2FleetMembersUnlinkHelper :<|> DeleteRegistrationV2FleetMembersHelper))

type PostRegistrationV2LoginOtp =
  ( "v2" :> "login" :> "otp" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text :> ReqBody '[JSON] FleetOwnerLoginReqV2
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostRegistrationV2V2LoginOtpHelper =
  ( "v2" :> "login" :> "otp" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text :> MandatoryQueryParam "enabled" Kernel.Prelude.Bool
      :> ReqBody
           '[JSON]
           FleetOwnerLoginReqV2
      :> Post '[JSON] FleetOwnerLoginResV2
  )

type PostRegistrationV2VerifyOtp = ("v2" :> "verify" :> "otp" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text :> ReqBody '[JSON] FleetOwnerVerifyReqV2 :> Post '[JSON] FleetOwnerVerifyResV2)

type PostRegistrationV2V2VerifyOtpHelper =
  ( "v2" :> "verify" :> "otp" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text :> ReqBody '[JSON] FleetOwnerVerifyReqV2
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostRegistrationV2Register = ("v2" :> "register" :> ReqBody '[JSON] FleetOwnerRegisterReqV2 :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostRegistrationV2V2RegisterHelper =
  ( "v2" :> "register" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] FleetOwnerRegisterReqV2
      :> Post
           '[JSON]
           FleetOwnerRegisterResV2
  )

type GetRegistrationV2FleetMembers =
  ( "fleet" :> "members" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam "to" Kernel.Prelude.UTCTime
      :> Get '[JSON] FleetMemberAssociationRes
  )

type GetRegistrationV2FleetMembersHelper =
  ( "fleet" :> "members" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam "to" Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           FleetMemberAssociationRes
  )

type PostRegistrationV2FleetMembersUnlink = ("fleet" :> "members" :> Capture "fleetMemberId" Kernel.Prelude.Text :> "unlink" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostRegistrationV2FleetMembersUnlinkHelper =
  ( "fleet" :> "members" :> Capture "fleetMemberId" Kernel.Prelude.Text :> "unlink" :> Capture "fleetOwnerId" Kernel.Prelude.Text
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type DeleteRegistrationV2FleetMember = ("fleet" :> "member" :> Capture "fleetMemberId" Kernel.Prelude.Text :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess)

type DeleteRegistrationV2FleetMembersHelper =
  ( "fleet" :> "members" :> Capture "fleetMemberId" Kernel.Prelude.Text :> Capture "fleetOwnerId" Kernel.Prelude.Text
      :> Delete
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data RegistrationV2APIs = RegistrationV2APIs
  { postRegistrationV2LoginOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Bool -> FleetOwnerLoginReqV2 -> EulerHS.Types.EulerClient FleetOwnerLoginResV2,
    postRegistrationV2VerifyOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> FleetOwnerVerifyReqV2 -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postRegistrationV2Register :: Kernel.Prelude.Text -> FleetOwnerRegisterReqV2 -> EulerHS.Types.EulerClient FleetOwnerRegisterResV2,
    getRegistrationV2FleetMembers :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient FleetMemberAssociationRes,
    postRegistrationV2FleetMembersUnlink :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteRegistrationV2FleetMember :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkRegistrationV2APIs :: (Client EulerHS.Types.EulerClient API -> RegistrationV2APIs)
mkRegistrationV2APIs registrationV2Client = (RegistrationV2APIs {..})
  where
    postRegistrationV2LoginOtp :<|> postRegistrationV2VerifyOtp :<|> postRegistrationV2Register :<|> getRegistrationV2FleetMembers :<|> postRegistrationV2FleetMembersUnlink :<|> deleteRegistrationV2FleetMember = registrationV2Client

data RegistrationV2UserActionType
  = POST_REGISTRATION_V2_LOGIN_OTP
  | POST_REGISTRATION_V2_VERIFY_OTP
  | POST_REGISTRATION_V2_REGISTER
  | GET_REGISTRATION_V2_FLEET_MEMBERS
  | POST_REGISTRATION_V2_FLEET_MEMBERS_UNLINK
  | DELETE_REGISTRATION_V2_FLEET_MEMBER
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''RegistrationV2UserActionType])
